package day8
import scala.NotImplementedError

case class MachineState(val accumulator: Int, val ip: Int) {
  def step(): MachineState = MachineState(accumulator, ip + 1)
  def jump(value: Int): MachineState = MachineState(accumulator, ip + value)
  def incr(value: Int): MachineState = MachineState(accumulator + value, ip)
}

sealed trait Instruction {
  def perform(state: MachineState): MachineState
}

object Instruction {
  def parseLine(line: String): Instruction = {
    val parts = line.split(' ')
    val operand = parts(1).toInt

    parts(0) match {
      case "nop" => Nop(operand)
      case "acc" => Acc(operand)
      case "jmp" => Jmp(operand)
      case _ => throw new NotImplementedError
    }
  }
}

case class Nop(val operand: Int) extends Instruction {
  def perform(state: MachineState) = state.step
}

case class Acc(val operand: Int) extends Instruction {
  def perform(state: MachineState) = state.step.incr(operand)
}

case class Jmp(val operand: Int) extends Instruction {
  def perform(state: MachineState) = state.jump(operand)
}

object Program {
  def apply(input: Iterator[String]) = {
    new Program(input.map(Instruction.parseLine).toVector, MachineState(0, 0))
  }
}

class Program(val instructions: Vector[Instruction], var state: MachineState) {
  var seen: Set[Int] = Set()

  // returns true if we terminated naturally, false if we repeat an instruction
  def runUntilRepeat(): Boolean = {
    while (!seen(state.ip)) {
      seen = seen + state.ip
      state = instructions(state.ip).perform(state)

      if (state.ip == instructions.size)
        return true
    }

    false
  }
}

class ProgramSet(programs: List[Program]) {
  def findTerminating(): Option[Program] = programs.find(_.runUntilRepeat())
}

object ProgramSet {
  def apply(input: Iterator[String]) = {
    // base program not included in the output since we know it wont work
    val baseProgram = input.map(Instruction.parseLine _).toIndexedSeq
    val programs = baseProgram
      .zipWithIndex
      .map {
        case (Nop(v), index) => Some(patch(baseProgram, index, Jmp(v)))
        case (Jmp(v), index) => Some(patch(baseProgram, index, Nop(v)))
        case _ => None
      }
      .flatten
      .map(p => new Program(p.toVector, MachineState(0, 0)))
      .toList

    new ProgramSet(programs)
  }

  // duplicate a sequence of instructions, patching the instruction at offset with instr
  private
  def patch(base: IndexedSeq[Instruction], offset: Int, newInstr: Instruction): IndexedSeq[Instruction] = {
    base
      .zipWithIndex
      .map {
        case (oldInstr, `offset`) => newInstr
        case (oldInstr, _) => oldInstr
      }
  }
}
