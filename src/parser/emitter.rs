use crate::parser::ast::*;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::IntPredicate;
use inkwell::AddressSpace;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue, FunctionValue};
use inkwell::types::BasicTypeEnum;
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
    fn push_scope_mark(&mut self, identifier: String) -> String {
        let mut rng = thread_rng();
        let random_chars: String = iter::repeat(()).map(|()| rng.sample(Alphanumeric)).take(7).collect();
        let scope_identifier = identifier + "_" + &random_chars;
        let pointer = self.builder.build_alloca(self.context.i32_type(), &scope_identifier);
        self.variables.update(scope_identifier.clone(), pointer);
        scope_identifier

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

        let parameters: Vec<BasicTypeEnum> = function.parameters.iter()
                                              .map(|_| self.context.i32_type().into())
                                              .collect();
        let func = self.module.add_function(&identifier, self.context.i32_type().fn_type(&parameters, false), None);
        self.functions.insert(identifier.clone(), func);
        let basic_block = self.context.append_basic_block(&func, "entry");

        self.builder.position_at_end(&basic_block);
        let scope_identifier = self.push_scope_mark(identifier);
        
        for (i, parameter_identifier) in function.parameters.into_iter().enumerate() {
            let parameter_value = match func.get_nth_param(i as u32) {
                Some(val) => val.into_int_value(),
                None => panic!("sippai"),
            };
            let parameter_alloca = self.builder.build_alloca(self.context.i32_type(), &parameter_identifier);
            self.builder.build_store(parameter_alloca, parameter_value);
            self.variables.update(parameter_identifier, parameter_alloca);
        }

        for ast in function.statements {
            match self.emit_ast_statement(ast, func) {
                Some(_) => (),
                None => break,
            }
        }
        self.variables.pop_to_identifier(scope_identifier);
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
            AstInstructionStatement::Declare(ast) => {
                let ret_value = self.emit_ast_declare(ast);
                Some(ret_value)
            },
        }
    }
    fn emit_ast_compound_statement(&mut self, ast: AstCompoundStatement) -> Option<IntValue> {
        let state_identifier = self.push_scope_mark("compound".to_string());
        let pointer = self.builder.build_alloca(self.context.i32_type(), &state_identifier);
        self.variables.update(state_identifier.clone(), pointer);

        let AstCompoundStatement::Instructions(asts) = ast;
        let mut val = None;
        for ast in asts {
            match self.emit_ast_instruction(ast) {
                Some(v) => val = Some(v),
                None => {
                    self.variables.pop_to_identifier(state_identifier);
                    return None
                },
            }
        }
        self.variables.pop_to_identifier(state_identifier);
        val
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
        let block_pattern = match *block {
            AstStatement::CompoundStatement(ast) => self.emit_ast_compound_statement(ast),
            _ => panic!("this pattern is not implemented"),
        };
        if let Some(_) = block_pattern {
            self.builder.build_unconditional_branch(&cont_block);
        }
        self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(&else_block);
        self.builder.build_unconditional_branch(&cont_block);
        self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(&cont_block);
        if let Some(_) = block_pattern {
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
    fn emit_ast_declare(&mut self, ast: AstDeclare) -> IntValue {
        match ast {
            AstDeclare::SimpleDeclare(ast) => self.emit_ast_simple_declare(ast),
            AstDeclare::BindDeclare(ast) => self.emit_ast_bind_declare(ast),
        }
    }
    fn emit_ast_simple_declare(&mut self, ast: AstSimpleDeclare) -> IntValue {
        let const_zero = self.context.i32_type().const_int(0, false);
        match ast.ide {
            AstIde::Num(ast) => {
                let identifier = ast.identifier;
                let alloca = match self.variables.get(&identifier) {
                    Some(_) => panic!(format!("redefinition of {:?}", identifier)),
                    None => self.builder.build_alloca(self.context.i32_type(), &identifier),
                };
                self.variables.update(identifier, alloca);
                const_zero
            },
            AstIde::Arr(ast) => {
                let identifier = ast.identifier;
                let array_type = self.context.i32_type().array_type(ast.num);
                let alloca = match self.variables.get(&identifier) {
                    Some(_) => panic!(format!("redefinition of {:?}", identifier)),
                    None => self.builder.build_alloca(array_type, &identifier),
                };
                self.variables.update(identifier, alloca);
                const_zero
            },
        }
    }
    fn emit_ast_bind_declare(&mut self, ast: AstBindDeclare) -> IntValue {
        match ast.ide {
            AstIde::Num(numide) => {
                let identifier = numide.identifier;
                let alloca = match self.variables.get(&identifier) {
                    Some(_) => panic!(format!("redefinition of {:?}", identifier)),
                    None => self.builder.build_alloca(self.context.i32_type(), &identifier),
                };
                let val = self.emit_ast_val(ast.val);
                self.builder.build_store(alloca, val);
                self.variables.update(identifier, alloca);
                val
            },
            AstIde::Arr(_arride) => panic!("not impelemented"), // TODO: impl: int a[3] =  {1, 2, 3};
        }
    }
    fn emit_ast_bind(&mut self, ast: AstBinding) -> IntValue {
        match ast.ide {
            AstIde::Num(numide) => {
                let identifier = numide.identifier;
                let alloca = match self.variables.get(&identifier) {
                    Some(alloca) => alloca,
                    None => panic!(format!("use of undeclared identifier \"{:?}\".", identifier)),
                };
                let val = self.emit_ast_val(ast.val);
                self.builder.build_store(alloca, val);
                self.variables.update(identifier, alloca);
                val
            },
            AstIde::Arr(arr_ast) => {
                let identifier = arr_ast.identifier;
                let alloca = match self.variables.get(&identifier) {
                    Some(alloca) => alloca,
                    None => panic!(format!("use of undeclared identifier \"{:?}\".", identifier)),
                };
                let ptr = unsafe {
                    self.builder.build_gep(
                        alloca,
                        &[self.context.i32_type().const_int(0, false), self.context.i32_type().const_int(arr_ast.num as u64, false)],
                        "insert"
                        )
                };
                let val = self.emit_ast_val(ast.val);
                self.builder.build_store(ptr, val);
                self.variables.update(identifier, alloca);
                val
            },
        }
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
        // TODO: impl error handling
        match ast_ide {
            AstIde::Num(ast) => self.builder.build_load(self.variables.get(&ast.identifier).unwrap(), &ast.identifier).into_int_value(),
            AstIde::Arr(ast) => {
                let array = self.builder.build_load(self.variables.get(&ast.identifier).unwrap(), &ast.identifier).into_array_value();
                // TODO: consider whether to use GEP or ExtractValue
                self.builder.build_extract_value(array, ast.num, "extracted_value").unwrap().into_int_value()
            },
        }
    }
    fn emit_ast_call(&mut self, ast: AstCall) -> IntValue {
        let identifier = ast.func_identifier;
        let functions = self.functions.clone();
        let fn_value = functions.get(&identifier).expect("Emit Error: Undeclared function.");
        let arguments: Vec<BasicValueEnum> = ast.arguments.into_iter()
                                              .map(|val| self.emit_ast_val(val))
                                              .map(|val| val.into())
                                              .collect();
        let func_call_site = self.builder.build_call(*fn_value, &arguments, "run_func");
        func_call_site.try_as_basic_value().left().unwrap().into_int_value()
    }
}
