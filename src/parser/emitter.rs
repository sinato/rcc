use crate::parser::ast::*;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::IntPredicate;
use inkwell::values::{IntValue, PointerValue, FunctionValue};
use std::collections::HashMap;
use std::path;

#[derive(Clone)]
struct Environment {
    variables: HashMap<String, PointerValue>
}
impl Environment {
    fn new() -> Environment {
        Environment{ variables: HashMap::new() }
    }
    fn get(&self, s: &str) -> Option<&PointerValue> {
        self.variables.get(s)
    }
    fn insert(&mut self, identifier: String, pointer: PointerValue) {
        self.variables.insert(identifier, pointer);
    }
}

pub struct Emitter {
    context: Context,
    builder: Builder,
    module: Module,
    variables: Environment
}
impl Emitter {
    pub fn new() -> Emitter {
        let context = Context::create();
        let module = context.create_module("my_module");
        let builder = context.create_builder();
        let variables = Environment::new();

        Emitter {
            context,
            builder, 
            module,
            variables
        }
    }
    pub fn print_to_file(&self, filename: &str) {
        let _ = self.module.print_to_file(path::Path::new(filename));
    }
    pub fn emit(&mut self, function: AstFunction) {
        self.emit_function(function)
    }
    fn emit_function(&mut self, function: AstFunction) {
        let func = self.module.add_function("main", self.context.i32_type().fn_type(&[], false), None);
        let basic_block = self.context.append_basic_block(&func, "entry");
        self.builder.position_at_end(&basic_block);

        let asts = function.statements.clone();
        for ast in asts {
            match self.emit_ast_statement(ast, func).0 {
                Some(_) => (),
                None => break,
            }
        }
    }
    fn emit_ast_statement(&mut self, ast_node: AstStatement, function: FunctionValue) -> (Option<IntValue>, Environment) {
        let (ret_val, ret_env) = match ast_node {
            AstStatement::Instruction(ast) => self.emit_ast_instruction(ast),
            AstStatement::CompoundStatement(ast) => self.emit_ast_compound_statement(ast),
            AstStatement::IfStatement(ast) => self.emit_ast_if_statement(ast, function),
        };
        self.variables = ret_env.clone();
        (ret_val, ret_env)
    }
    fn emit_ast_instruction(&mut self, ast_node: AstInstruction) -> (Option<IntValue>, Environment) {
        let statement_environment = self.variables.clone();
        match ast_node {
            AstInstruction::Bind(ast) => {
                let (ret_value, ret_environment) = self.emit_ast_bind(ast, statement_environment);
                (Some(ret_value), ret_environment)
            },
            AstInstruction::Return(ast) => { 
                let (_, ret_environment) = self.emit_ast_return(ast, statement_environment);
                (None, ret_environment) 
            },
        }
    }
    fn emit_ast_compound_statement(&mut self, ast: AstCompoundStatement) -> (Option<IntValue>, Environment) {
        let mut statement_environment = self.variables.clone();
        // TODO: refactoring
        let AstCompoundStatement::Instructions(asts) = ast;
        let mut val = None;

        for ast in asts {
            let (ret_val, ret_env) = self.emit_ast_instruction(ast);
            match ret_val {
                Some(v) => {
                    val = Some(v);
                    for (identifier, pointer) in ret_env.variables.into_iter() {
                        statement_environment.insert(identifier, pointer);
                    }
                },
                None => panic!("Return is not allowed in a block."),
            };
        }
        match val {
            Some(_) => (val, statement_environment),
            None => panic!("This block has no statements."),
        }
    }
    fn emit_ast_if_statement(&mut self, ast: AstIfStatement, function: FunctionValue) -> (Option<IntValue>, Environment) {
        let mut statement_environment = self.variables.clone();
        let block = ast.block;

        let const_one = self.context.i32_type().const_int(0, false);
        let cond = self.emit_ast_val(ast.condition_val);

        let cond = self.builder.build_int_compare(IntPredicate::EQ, cond, const_one, "ifcond");

        let then_block = self.context.append_basic_block(&function, "then");
        let else_block = self.context.append_basic_block(&function, "else");
        let cont_block = self.context.append_basic_block(&function, "cont");

        self.builder.build_conditional_branch(cond, &then_block, &else_block);
        self.builder.position_at_end(&then_block);

        let (then_val, ret_env) = self.emit_ast_statement(*block, function);
        let then_val = then_val.unwrap();
        self.builder.build_unconditional_branch(&cont_block);
        self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(&else_block);
        // emit something "else"
        self.builder.build_unconditional_branch(&cont_block);
        self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(&cont_block);
        let phi = self.builder.build_phi(self.context.i32_type(), "iftmp");

        for (identifier, pointer) in ret_env.variables.into_iter() {
            statement_environment.insert(identifier, pointer);
        }

        phi.add_incoming(&[
                         (&then_val, &then_block),
                         (&const_one, &else_block),
        ]);
        (Some(phi.as_basic_value().into_int_value()), statement_environment)
    }
    fn emit_ast_bind(&mut self, ast_binding: AstBinding, mut environment: Environment) -> (IntValue, Environment) {
        let identifier = ast_binding.ide.get_identifier();
        let pointer = self.builder.build_alloca(self.context.i32_type(), &identifier);
        let val = self.emit_ast_val(ast_binding.val);

        self.builder.build_store(pointer, val);
        environment.insert(identifier, pointer);
        (val, environment)
    }
    fn emit_ast_return(&mut self, ast_ret: AstReturn, environment: Environment) -> (IntValue, Environment) {
        let statement_environment = environment.clone();
        let ret = self.emit_ast_val(ast_ret.val);
        self.builder.build_return(Some(&ret));
        (ret, statement_environment)
    }
    fn emit_ast_val(&mut self, ast_val: AstVal) -> IntValue {
        match ast_val {
            AstVal::Exp(ast) => self.emit_ast_exp(ast),
            AstVal::Fin(ast) => self.emit_ast_fin(ast),
        }
    }
    fn emit_ast_exp(&mut self, ast_binary_exp: AstExp) -> IntValue {
        let lhs_num = self.emit_ast_val(*ast_binary_exp.lhs.clone());
        let rhs_num = self.emit_ast_val(*ast_binary_exp.rhs.clone());
        let op = ast_binary_exp.get_op_string();
        match op.as_ref() {
            "+" => self.builder.build_int_add(lhs_num, rhs_num, "sum"),
            "*" => self.builder.build_int_mul(lhs_num, rhs_num, "mul"),
            _ => panic!("Emit Error: Not implemented operator"),
        }
    }
    fn emit_ast_fin(&self, ast_fin: AstFin) -> IntValue {
        match ast_fin {
            AstFin::Num(ast) => self.emit_ast_num(ast),
            AstFin::Ide(ast) => self.emit_ast_ide(ast),
        }
    }
    fn emit_ast_num(&self, ast_num: AstNum) -> IntValue {
        self.context.i32_type().const_int(ast_num.num.get_num(), false)
    }
    fn emit_ast_ide(&self, ast_ide: AstIde) -> IntValue {
        let allocation = self.variables.get(&ast_ide.get_identifier());
        match allocation {
            Some(pointer) => self.builder.build_load(*pointer, &ast_ide.ide.get_ide()).into_int_value(),
            None => panic!("Emit Error: Undeclared variable."),
        }
    }
}
