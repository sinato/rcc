use crate::parser::ast::*;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{IntValue, PointerValue};
use std::collections::HashMap;
use std::path;

pub struct Emitter {
    context: Context,
    builder: Builder,
    module: Module,
    variables: HashMap<String, PointerValue>
}
impl Emitter {
    pub fn new() -> Emitter {
        let context = Context::create();
        let module = context.create_module("my_module");
        let builder = context.create_builder();
        let variables = HashMap::new();

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

        let asts = function.instructions.clone();
        let ret = asts.into_iter().map(|ast| self.emit_ast_instruction(ast)).last();
        match ret {
            Some(ret) => self.builder.build_return(Some(&ret)),
            None => panic!("Emit Error: Expect at least an ast."),
        };
    }
    fn emit_ast_instruction(&mut self, ast_node: AstInstruction) -> IntValue {
        match ast_node {
            AstInstruction::Bind(ast) => self.emit_ast_bind(ast),
            AstInstruction::Return(ast) => self.emit_ast_return(ast),
        }
    }
    fn emit_ast_bind(&mut self, ast_binding: AstBinding) -> IntValue {
        let identifier = ast_binding.ide.get_identifier();
        let pointer = self.builder.build_alloca(self.context.i32_type(), &identifier);
        let val = self.emit_ast_val(*ast_binding.val);

        self.builder.build_store(pointer, val);
        self.variables.insert(identifier, pointer);
        val
    }
    fn emit_ast_return(&mut self, ast_ret: AstReturn) -> IntValue {
        let ret = self.emit_ast_val(*ast_ret.val);
        self.builder.build_return(Some(&ret));
        ret
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


