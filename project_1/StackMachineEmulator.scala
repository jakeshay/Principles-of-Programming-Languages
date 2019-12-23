package edu.colorado.csci3155.project1
import scala.math._


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] =
        ins match {
            case PushI(f) => {
                f :: stack
            }
            case PopI => {
                if (stack.isEmpty == true) {
                    throw new IllegalArgumentException("stack already empty")
                }
                stack.drop(1)
            }
            case AddI => {
                val v1 = stack.head
                var nL = stack.drop(1)
                val v2 = nL.head
                nL = nL.drop(1)
                nL = emulateSingleInstruction(nL, PushI(v2 + v1))
                nL
            }
            case SubI => {
                val v1 = stack.head
                var nL = stack.drop(1)
                val v2 = nL.head
                nL = nL.drop(1)
                nL = emulateSingleInstruction(nL, PushI(v2 - v1))
                nL
            }
            case MultI => {
                val v1 = stack.head
                var nL = stack.drop(1)
                val v2 = nL.head
                nL = nL.drop(1)
                nL = emulateSingleInstruction(nL, PushI(v2 * v1))
                nL
            }
            case DivI => {
                val v1 = stack.head
                var nL = stack.drop(1)
                val v2 = nL.head
                nL = nL.drop(1)
                nL = emulateSingleInstruction(nL, PushI(v2 / v1))
                nL
            }
            case ExpI => {
                val v1 = stack.head
                var nL = stack.drop(1)
                nL = emulateSingleInstruction(nL, PushI(exp(v1)))
                nL
            }
            case LogI => {
                val v1 = stack.head
                if (v1 <= 0) {
                    throw new IllegalArgumentException("cannot take log of non-positive")
                }
                var nL = stack.drop(1)
                nL = emulateSingleInstruction(nL, PushI(log(v1)))
                nL
            }
            case SinI => {
                val v1 = stack.head
                var nL = stack.drop(1)
                nL = emulateSingleInstruction(nL, PushI(sin(v1)))
                nL
            }
            case CosI => {
                val v1 = stack.head
                var nL = stack.drop(1)
                nL = emulateSingleInstruction(nL, PushI(cos(v1)))
                nL
            }
        }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double = {
        var stack = List[Double]()
        for (e <- instructionList) stack = emulateSingleInstruction(stack, e)

        stack.last
    }
}