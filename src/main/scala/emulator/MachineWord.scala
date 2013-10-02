package emulator


/*
 * Even though MachineWord is byte-sized its modeled on a Int because this code
 * is intented to work on both the Scala.js but also the JVM
 * Due to the way Scala.js handles all integer types on top of a Javascript Number (double),
 * and how bitwise ops work in Javascript (the double is converted to a 32 bit int before applying), 
 * using an Int makes sense, to unify the semantics of the different runtimes.
 * NOTE: only tested on Scala.js so far.
 */
class MachineWord private (val storage: Int) extends AnyVal {
  
  def floatComponents = {
    val signbit = ((storage >>> 7) & 0x1)
    val exponent_excess = ((storage >>> 4) & 0x7)
    val mantissa = storage & 0xf
    (signbit, exponent_excess, mantissa)
  }
  
  def unsignedValue = storage
  
  def signedValue = {
    if (signbitSet) 
      storage | 0xffffff00  // sign extend
    else
      storage
  }
  
  def floatValue = {
    val (signbit, exponent_excess, mantissa) = floatComponents 
    val exponent = exponent_excess - 4
    
    var sum: Double = 0;
    
    for (i <- 0 to 3) {
      val bit = (mantissa >>> i) & 0x1 
      val e = i - 4;
      
      sum += bit * math.pow(2, e + exponent)
    }
    
    if (signbitSet) -sum else sum
  }
  
  private def signbitSet = floatComponents._1 == 1
  
  def toHexString = f"$unsignedValue%02X"
  
  override def toString = f"MachineWord(0x$toHexString)"
}

object MachineWord {
  import language.implicitConversions
  
  implicit def unsignedToWord(x: Int) = MachineWord(x)
  implicit def wordToUnsigned(x: MachineWord) = x.unsignedValue
  def apply(v: Int) = new MachineWord(v & 0xff)
}