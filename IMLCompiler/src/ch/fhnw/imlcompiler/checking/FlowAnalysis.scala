package ch.fhnw.imlcompiler.checking

import ch.fhnw.imlcompiler.checking.ProgramContext._
import ch.fhnw.imlcompiler.AST.Program

// TODO implement flow analysis (init with branches)
trait FlowAnalysis {
  def ceckFlow(program: Program, context: Context) = {}
}