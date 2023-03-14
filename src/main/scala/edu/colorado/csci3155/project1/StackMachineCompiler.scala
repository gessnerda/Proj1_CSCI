package edu.colorado.csci3155.project1

object StackMachineCompiler {

    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match {
            case Const(c) => List(PushNumI(c))
            case Plus(a, b) => compileToStackMachineCode(a) ++ compileToStackMachineCode(b) ++ List(AddI)
            case Minus(a, b) => compileToStackMachineCode(a) ++ compileToStackMachineCode(b) ++ List(SubI)
            case Mult(a, b) => compileToStackMachineCode(a) ++ compileToStackMachineCode(b) ++ List(MultI)
            case Div(a, b) => compileToStackMachineCode(a) ++ compileToStackMachineCode(b) ++ List(DivI)
            case Ident(id) => List(LoadEnv(id))
            case Let(id, e1, e2) =>
                compileToStackMachineCode(e1) ++
                    List(StoreEnv(id)) ++
                    compileToStackMachineCode(e2) ++
                    List(PopEnv)

            
            case IfThenElse(cond, thenPart, elsePart) =>
                val thenInstructions = compileToStackMachineCode(thenPart)
                val elseInstructions = compileToStackMachineCode(elsePart)
                val condInstructions = compileToStackMachineCode(cond)
                condInstructions ++
                    List(CSkipI(thenInstructions.length + 1)) ++
                    thenInstructions ++
                    List(SkipI(elseInstructions.length)) ++
                    elseInstructions
            case Geq(a, b) => compileToStackMachineCode(a) ++ compileToStackMachineCode(b) ++ List(GeqI)
            case And(a, b) =>
                val aInstructions = compileToStackMachineCode(a)
                val bInstructions = compileToStackMachineCode(b)
                aInstructions ++ List(CSkipI(bInstructions.length + 1)) ++ bInstructions ++ List(GeqI, PushNumI(1.0), GeqI)
            case Or(a, b) =>
                val aInstructions = compileToStackMachineCode(a)
                val bInstructions = compileToStackMachineCode(b)
                aInstructions ++ List(CSkipI(aInstructions.length + 1)) ++ bInstructions ++ List(AddI, GeqI, NotI)
            case Not(a) => compileToStackMachineCode(a) ++ List(NotI)
            case Exp(a) => compileToStackMachineCode(a) ++ List(ExpI)
            case Log(a) => compileToStackMachineCode(a) ++ List(LogI)
            case Sine(a) => compileToStackMachineCode(a) ++ List(SinI)
            case Cosine(a) => compileToStackMachineCode(a) ++ List(CosI)
            case _ => throw new UnsupportedOperationException(s"Unsupported expression: $e")
        }
    }
}
