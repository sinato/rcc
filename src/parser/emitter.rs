use crate::parser::ast::*;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::IntPredicate;
use inkwell::AddressSpace;
use inkwell::values::{IntValue, PointerValue, FunctionValue};
use std::collections::HashMap;
use std::path;
use std::iter;
use rand::{Rng, thread_rng};
use rand::distributions::Alphanumeric;

#[derive(Debug, Clone)]
struct Environment {
    variables: Vec<(String, PointerValue)>
}
impl Environment {
    fn new() -> Environment {
        let variables: Vec<(String, PointerValue)> = Vec::new();
        Environment{ variables }
    }
    fn get(&self, skey: &String) -> Option<PointerValue> {
        match self.variables.iter().rev().find(|x| &x.0 == skey) {
            Some(val) => Some(val.1),
            None => None,
        }
    }
    fn find(&self, skey: &String) -> Option<usize> {
        match self.variables.iter().rev().position(|x| &x.0 == skey) {
            Some(idx) => Some(self.variables.len() - idx - 1),
            None => None,
        }
    }
    fn update(&mut self, skey: String, sval: PointerValue) {
        match self.find(&skey) {
            Some(idx) => self.variables[idx] = (skey, sval),
            None => self.variables.push((skey, sval)),
        }
    }
    fn pop_to_identifier(&mut self, skey: String) {
        while let Some((key, _val)) = self.variables.pop() {
            if key == skey {
                break;
            }
        }
    }
}

fn add_rnd_identifier(identifier: String) -> String {
    let mut rng = thread_rng();
    let random_chars: String = iter::repeat(()).map(|()| rng.sample(Alphanumeric)).take(7).collect();
    identifier + &random_chars
}


pub struct Emitter {
    context: Context,
    builder: Builder,
    module: Module,
    variables: Environment,
    functions: HashMap<String, FunctionValue>
}
impl Emitter {
    pub fn new() -> Emitter {
        let context = Context::create();
        let module = context.create_module("my_module");
        let builder = context.create_builder();
        let variables = Environment::new();
        let functions = HashMap::new();

        Emitter {
            context,
            builder, 
            module,
            variables,
            functions,
        }
    }
    pub fn print_to_file(&self, filename: &str) {
        let _ = self.module.print_to_file(path::Path::new(filename));
    }
    pub fn emit(&mut self, program: AstProgram) {
        self.emit_program(program)
    }
    fn emit_program(&mut self, program: AstProgram) {
        for function in program.functions {
            self.emit_function(function)
        }
    }
    fn emit_function(&mut self, function: AstFunction) {
        let identifier = function.identifier;
        let func = self.module.add_function(&identifier, self.context.i32_type().fn_type(&[], false), None);
        self.functions.insert(identifier, func);

        let basic_block = self.context.append_basic_block(&func, "entry");
        self.builder.position_at_end(&basic_block);
        for ast in function.statements {
            match self.emit_ast_statement(ast, func) {
                Some(_) => (),
                None => break,
            }
        }
    }
    fn emit_ast_statement(&mut self, ast_node: AstStatement, function: FunctionValue) -> Option<IntValue> {
        let ret_val = match ast_node {
            AstStatement::InstructionStatement(ast) => self.emit_ast_instruction(ast),
            AstStatement::CompoundStatement(ast) => self.emit_ast_compound_statement(ast),
            AstStatement::IfStatement(ast) => self.emit_ast_if_statement(ast, function),
            AstStatement::WhileStatement(ast) => self.emit_ast_while_statement(ast, function),
        };
        ret_val
    }
    fn emit_ast_instruction(&mut self, ast_node: AstInstructionStatement) -> Option<IntValue> {
        match ast_node {
            AstInstructionStatement::Bind(ast) => {
                let ret_value = self.emit_ast_bind(ast);
                Some(ret_value)
            },
            AstInstructionStatement::Return(ast) => { 
                let _ = self.emit_ast_return(ast);
                None
            },
        }
    }
    fn emit_ast_compound_statement(&mut self, ast: AstCompoundStatement) -> Option<IntValue> {
        let state_identifier = add_rnd_identifier("compound".to_string());
        let pointer = self.builder.build_alloca(self.context.i32_type(), &state_identifier);
        self.variables.update(state_identifier.clone(), pointer);

        let AstCompoundStatement::Instructions(asts) = ast;
        let mut val = None;
        for ast in asts {
            let ret_val = self.emit_ast_instruction(ast);
            match ret_val {
                Some(v) => val = Some(v),
                None => panic!("Return is not allowed in a block."),
            };
        }
        self.variables.pop_to_identifier(state_identifier);
        match val {
            Some(_) => val,
            None => panic!("This block has no statements."),
        }
    }
    // https://thedan64.github.io/inkwell/inkwell/enum.IntPredicate.html
    fn emit_ast_condition_statement(&mut self, ast: AstConditionalStatement) -> IntValue {
        let val = self.emit_ast_val(ast.condition_val);
        let iden_val = self.emit_ast_ide(ast.condition_identifier);
        match ast.condition_operator.as_ref() {
            "==" => self.builder.build_int_compare(IntPredicate::EQ, iden_val, val, "ifcond"),
            "!=" => self.builder.build_int_compare(IntPredicate::NE, iden_val, val, "ifcond"),
            _ => panic!(format!("This operator is not implemented."),
        }
    }
    fn emit_ast_if_statement(&mut self, ast: AstIfStatement, function: FunctionValue) -> Option<IntValue> {
        let block = ast.block;

        let const_one = self.context.i32_type().const_int(1, false);

        let then_block = self.context.append_basic_block(&function, "then");
        let else_block = self.context.append_basic_block(&function, "else");
        let cont_block = self.context.append_basic_block(&function, "cont");
        let cond = self.emit_ast_condition_statement(ast.condition_statement);

        self.builder.build_conditional_branch(cond, &then_block, &else_block);
        self.builder.position_at_end(&then_block);

        let env = self.variables.clone();
        let _ = match *block {
            AstStatement::CompoundStatement(ast) => self.emit_ast_compound_statement(ast),
            _ => panic!("this pattern is not implemented"),
        };
        self.builder.build_unconditional_branch(&cont_block);
        self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(&else_block);
        self.builder.build_unconditional_branch(&cont_block);
        self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(&cont_block);
        // PointerValue -> IntValue ---> PhiValue -> IntValue -> PointerValue
        // PointerValue -> IntValue _/
        for (key, _) in env.variables.iter() {
            let alloca_val = env.get(&key).expect("");
            let alloca_then_val = self.variables.get(&key).expect("");

            let alloca_phi = self.builder.build_phi(self.context.i32_type().ptr_type(AddressSpace::Generic), "phitmp");
            alloca_phi.add_incoming(&[(&alloca_then_val, &then_block), (&alloca_val, &else_block)]);
            let alloca_phi = alloca_phi.as_basic_value().into_pointer_value();
            self.variables.update(key.to_string(), alloca_phi);
        }
        Some(const_one)
    }
    fn emit_ast_while_statement(&mut self, ast: AstWhileStatement, function: FunctionValue) -> Option<IntValue> {
        let block = ast.block;
        let const_one = self.context.i32_type().const_int(1, false);

        let loop_block = self.context.append_basic_block(&function, "loop");
        let cont_block = self.context.append_basic_block(&function, "cont");
        self.builder.build_unconditional_branch(&loop_block);

        self.builder.position_at_end(&loop_block);
        let _ = match *block {
            AstStatement::CompoundStatement(ast) => self.emit_ast_compound_statement(ast),
            _ => panic!("this pattern is not implemented"),
        };
        let cond = self.emit_ast_condition_statement(ast.condition_statement.clone());
        self.builder.build_conditional_branch(cond, &loop_block, &cont_block);

        self.builder.position_at_end(&cont_block);
        Some(const_one)
    }
    fn emit_ast_bind(&mut self, ast_binding: AstBinding) -> IntValue {
        let identifier = ast_binding.ide.identifier;

        let alloca = match self.variables.get(&identifier) {
            Some(alloca) => alloca,
            None => self.builder.build_alloca(self.context.i32_type(), &identifier),
        };
        let val = self.emit_ast_val(ast_binding.val);
        self.builder.build_store(alloca, val);
        self.variables.update(identifier, alloca);
        val
    }
    fn emit_ast_return(&mut self, ast_ret: AstReturn) -> IntValue {
        let ret = self.emit_ast_val(ast_ret.val);
        self.builder.build_return(Some(&ret));
        ret
    }
    fn emit_ast_val(&mut self, ast_val: AstVal) -> IntValue {
        match ast_val {
            AstVal::Exp(ast) => self.emit_ast_exp(ast),
            AstVal::Fin(ast) => self.emit_ast_fin(ast),
            AstVal::Call(ast) => self.emit_ast_call(ast),
        }
    }
    fn emit_ast_exp(&mut self, ast_binary_exp: AstExp) -> IntValue {
        let lhs_num = self.emit_ast_val(*ast_binary_exp.lhs.clone());
        let rhs_num = self.emit_ast_val(*ast_binary_exp.rhs.clone());
        let op = ast_binary_exp.op.operator;
        match op.as_ref() {
            "+" => self.builder.build_int_add(lhs_num, rhs_num, "sum"),
            "*" => self.builder.build_int_mul(lhs_num, rhs_num, "mul"),
            _ => panic!("Emit Error: Not implemented operator"),
        }
    }
    fn emit_ast_fin(&mut self, ast_fin: AstFin) -> IntValue {
        match ast_fin {
            AstFin::Num(ast) => self.emit_ast_num(ast),
            AstFin::Ide(ast) => self.emit_ast_ide(ast),
        }
    }
    fn emit_ast_num(&self, ast_num: AstNum) -> IntValue {
        self.context.i32_type().const_int(ast_num.number, false)
    }
    fn emit_ast_ide(&mut self, ast_ide: AstIde) -> IntValue {
        self.builder.build_load(self.variables.get(&ast_ide.identifier).unwrap(), &ast_ide.identifier).into_int_value()
    }
    fn emit_ast_call(&mut self, ast: AstCall) -> IntValue {
        let identifier = ast.func_identifier;
        let fn_value = self.functions.get(&identifier).expect("Emit Error: Undeclared function.");
        let func_call_site = self.builder.build_call(*fn_value, &[], "run_func");
        func_call_site.try_as_basic_value().left().unwrap().into_int_value()
    }
}
