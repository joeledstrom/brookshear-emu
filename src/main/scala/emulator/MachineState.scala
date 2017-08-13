package emulator

import MachineWord._

sealed trait MachineCycleState {
  def next: MachineCycleState
  def enter(st: MachineState): MachineState = st
}

case object Initial extends MachineCycleState {
  override def next = Fetch
}
case object Fetch extends MachineCycleState {
  override def enter(st: MachineState) =
    st.withUpdatedPC(st.pc + 2)
      .withUpdatedIR((
        st.memory(st.pc % st.memory.size),
        st.memory((st.pc + 1) % st.memory.size)
      ))

  override def next = Decode
}
case object Decode extends MachineCycleState {
  override def enter(st: MachineState) = st.withUpdatedDecodedInstruction(MachineInstruction.decode(st.ir))
  override def next = Execute
}
case object Execute extends MachineCycleState {
  override def enter(st: MachineState) = st.decodedInstruction.execute(st)
  override def next = Fetch
} 
case object Halted extends MachineCycleState {
  override def next = Halted
}

case class MachineState private (memory: Seq[MachineWord], gprs: Seq[MachineWord], pc: MachineWord, 
  ir: (MachineWord, MachineWord), decodedInstruction: MachineInstruction, cycleState: MachineCycleState) {
    
  require(memory.length == 256);
  require(gprs.length == 16)
  
  
  def withUpdatedMemory(address: MachineWord, value: MachineWord) = copy(memory = memory.updated(address, value))
  def withUpdatedPC(pc: MachineWord) = copy(pc = pc)
  def withUpdatedIR(ir: (MachineWord, MachineWord)) = copy(ir = ir)
  def withUpdatedGPR(grpNum: Int, value: MachineWord) = copy(gprs = gprs.updated(grpNum, value))
  def withUpdatedDecodedInstruction(i: MachineInstruction) = copy(decodedInstruction = i)
  def withUpdatedCycleState(s: MachineCycleState) = copy(cycleState = s)
  def withClearedCPU = new MachineState(memory, Vector.fill(16)(0), 0, (0,0), Undefined, Initial)
  
  def step = {
    val st = withUpdatedCycleState(cycleState.next)
    st.cycleState.enter(st)
  }
}

object MachineState {
  def apply() = new MachineState(Vector.fill(256)(0), Vector.fill(16)(0), 0, (0,0), Undefined, Initial)
  def apply(mem: Seq[MachineWord]) = new MachineState(mem, Vector.fill(16)(0), 0, (0,0), Undefined, Initial)
}