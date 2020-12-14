package day14

case class MachineState(registers: Map[Long, Long], mask: String) {
  def store(pos: Long, value: Long) : MachineState  = {
    val entry = (pos -> applyMask(value))
    MachineState(registers + entry, mask)
  }

  def storeV2(pos: Long, value: Long) : MachineState = {
    val n = genAddresses(pos).map(addr => (addr -> value))
    MachineState(registers ++ n, mask)
  }

  def storeMask(newMask: String) = MachineState(registers, newMask)

  // part 1
  def applyMask(value: Long) : Long = {
    mask
      .zipWithIndex
      .foldLeft(value)((v, pair) => {
        val (ch, index) = pair
        val bit = 35 - index
        ch match {
          case 'X' => v
          case '0' => v & ~(1L << bit)
          case '1' => v | (1L << bit)
        }
    })
  }

  // part 2:
  // input is the initial value provided
  // return value is list of addresses derived from input + mask
  def genAddresses(value: Long) : List[Long] = {
    mask
      .zipWithIndex
      .foldLeft(List(0L)) { (results, pair) => // initial list is a single entry of 0
        val (ch, index) = pair
        val bit = 35 - index // bit # of the currently considered bit in the input
        ch match {
          case 'X' => pushBit(results, 0L) ++ pushBit(results, 1L) // append both 0 and 1 (doubles list size)
          case '0' => pushBit(results, (value >> bit) & 1L) // append original bit
          case '1' => pushBit(results, 1L) // append bit of 1
        }
      }
  }

  // for each address in the list, shift left and append the provided bit
  def pushBit(list: List[Long], bit: Long) : List[Long] = {
    list.map(v => (v << 1) | bit)
  }

  def memorySum = registers.values.sum
}

trait Instruction {
  def run(state: MachineState) : MachineState
  def runV2(state: MachineState) : MachineState
}

object Instruction {
  val StoreRegex = raw"mem\[(\d+)\] = (\d+)".r

  def apply(line: String) = {
    if (line.startsWith("mask"))
      Mask(line.split(" = ")(1))
    else {
      val m = StoreRegex.findFirstMatchIn(line).get
      Store(m.group(1).toLong, m.group(2).toLong)
    }
  }
}

case class Store(pos: Long, value: Long) extends Instruction {
  def run(state: MachineState) = state.store(pos, value)
  def runV2(state: MachineState) = state.storeV2(pos, value)
}

case class Mask(value: String) extends Instruction {
  def run(state: MachineState) = state.storeMask(value)
  def runV2(state: MachineState) = run(state)
}


class Program(lines: Iterator[String]) {
  val instr = lines.map(line => Instruction(line)).toList
  var state = MachineState(Map(), "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")


  def run() : MachineState = instr.foldLeft(state)((s, instr) => instr.run(s))
  def runV2() : MachineState = instr.foldLeft(state)((s, instr) => instr.runV2(s))
}
