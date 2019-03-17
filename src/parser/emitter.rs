use crate::parser::ast::*;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::IntPredicate;
use inkwell::values::{IntValue, FunctionValue};
use std::collections::HashMap;
use std::path;

#[derive(Debug, Clone)]
struct Environment {
    variables_stack: Vec<HashMap<String, IntValue>>
}
impl Environment {
    fn new() -> Environment {
        let env1: HashMap<String, IntValue> = HashMap::new();
        Environment{ variables_stack: vec![env1] }
    }
    fn push_env(&mut self) {
        let env1: HashMap<String, IntValue> = HashMap::new();
        self.variables_stack.push(env1);
    } 
    fn pop_env(&mut self) {
        match self.variables_stack.pop() {
            Some(_) => (),
            None => panic!("Expect at least one environement.")
        }
    }
    fn get(&mut self, s: &str) -> Option<&IntValue> {
        self.variables_stack[0].get(s)
    }
    fn update_exists(&mut self, identifier: String, value: IntValue) -> Result<String, &str> {
        for variables in &mut self.variables_stack {
            if variables.contains_key(&identifier) {
                variables.insert(identifier.clone(), value);
                return Ok(identifier);
            }
        }
        Err("identifier not found")
    }
    fn insert(&mut self, identifier: String, value: IntValue) {
        // We search the existing binding at first.
        // Declaration cannot be distinguished from definition yet
        // println!("$$$$insert {:?}", identifier);
        match self.update_exists(identifier.clone(), value) {
            Ok(_) => (),
            Err(_) => {
                if let Some(last) = self.variables_stack.last_mut() {
                    last.insert(identifier, value);
                }
            },
        };
    }
    fn get_variables(&self) -> HashMap<String, IntValue> {
        self.variables_stack[0].clone()
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
        for (key, val) in ret_env.get_variables().into_iter() {
            self.variables.insert(key, val);
        }
        /*
        println!("************");
        for (key, _) in self.variables.get_variables().iter() {
            println!("{:?}", key);
        }
        println!("************");
        */
        (ret_val, ret_env)
    }
    fn emit_ast_instruction(&mut self, ast_node: AstInstruction) -> (Option<IntValue>, Environment) {
        match ast_node {
            AstInstruction::Bind(ast) => {
                let (ret_value, ret_environment) = self.emit_ast_bind(ast);
                (Some(ret_value), ret_environment)
            },
            AstInstruction::Return(ast) => { 
                let (_, ret_environment) = self.emit_ast_return(ast);
                (None, ret_environment) 
            },
        }
    }
    fn emit_ast_compound_statement(&mut self, ast: AstCompoundStatement) -> (Option<IntValue>, Environment) {
        let mut statement_environment = self.variables.clone();
        statement_environment.push_env();

        // TODO: refactoring
        let AstCompoundStatement::Instructions(asts) = ast;
        let mut val = None;
        for ast in asts {
            let (ret_val, ret_env) = self.emit_ast_instruction(ast);
            match ret_val {
                Some(v) => {
                    val = Some(v);
                    for (identifier, pointer) in ret_env.get_variables().into_iter() {
                        statement_environment.insert(identifier, pointer);
                    }
                },
                None => panic!("Return is not allowed in a block."),
            };
        }
        statement_environment.pop_env();
        match val {
            Some(_) => (val, statement_environment),
            None => panic!("This block has no statements."),
        }
    }
    fn emit_ast_condition_statement(&mut self, ast: AstConditionalStatement) -> IntValue {
        let val = self.emit_ast_val(ast.condition_val);
        let iden_val = self.emit_ast_ide(ast.condition_identifier);
        self.builder.build_int_compare(IntPredicate::EQ, iden_val, val, "ifcond")
    }
    fn emit_ast_if_statement(&mut self, ast: AstIfStatement, function: FunctionValue) -> (Option<IntValue>, Environment) {
        let mut statement_environment = self.variables.clone();
        let block = ast.block;

        let const_one = self.context.i32_type().const_int(1, false);

        let then_block = self.context.append_basic_block(&function, "then");
        let else_block = self.context.append_basic_block(&function, "else");
        let cont_block = self.context.append_basic_block(&function, "cont");
        let cond = self.emit_ast_condition_statement(ast.condition_statement);

        self.builder.build_conditional_branch(cond, &then_block, &else_block);
        self.builder.position_at_end(&then_block);

        let (_, mut ret_env) = match *block {
            AstStatement::CompoundStatement(ast) => self.emit_ast_compound_statement(ast),
            _ => panic!("this pattern is not implemented"),
        };
        self.builder.build_unconditional_branch(&cont_block);
        self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(&else_block);
        self.builder.build_unconditional_branch(&cont_block);
        self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(&cont_block);
        // IntValue ---> PhiValue -> IntValue
        // IntValue _/
        for key in ret_env.get_variables().keys() {
            let val = self.variables.get(key).expect("");
            let then_val = ret_env.get(key).expect("");
            let phi_value = self.builder.build_phi(self.context.i32_type(), "phitmp");
            phi_value.add_incoming(&[(then_val, &then_block), (val, &else_block)]);
            statement_environment.insert(key.to_string(), phi_value.as_basic_value().into_int_value());
        }
        (Some(const_one), statement_environment)
    }
    fn emit_ast_bind(&mut self, ast_binding: AstBinding) -> (IntValue, Environment) {
        let mut statement_environment = Environment::new();
        let identifier = ast_binding.ide.get_identifier();

        let val = self.emit_ast_val(ast_binding.val);
        statement_environment.insert(identifier, val);
        (val, statement_environment)
    }
    fn emit_ast_return(&mut self, ast_ret: AstReturn) -> (IntValue, Environment) {
        let statement_environment = self.variables.clone();
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
    fn emit_ast_fin(&mut self, ast_fin: AstFin) -> IntValue {
        match ast_fin {
            AstFin::Num(ast) => self.emit_ast_num(ast),
            AstFin::Ide(ast) => self.emit_ast_ide(ast),
        }
    }
    fn emit_ast_num(&self, ast_num: AstNum) -> IntValue {
        self.context.i32_type().const_int(ast_num.num.get_num(), false)
    }
    fn emit_ast_ide(&mut self, ast_ide: AstIde) -> IntValue {
        let val = self.variables.get(&ast_ide.get_identifier());
        match val {
            Some(val) => *val,
            None => panic!("Emit Error: Undeclared variable."),
        }
    }
}
