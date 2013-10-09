package ch.fhnw.imlcompiler

import ch.fhnw.imlcompiler.AST.Program

trait ContextChecker {
	def check(prog:Program) {}
	
	object TypeChecker extends ContextChecker {
	}
}