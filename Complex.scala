class Complex(r: Double, u: Double) {
  
  val real1: Double = r
  val unreal: Double = u
  //println("Created " + r + "+" + u + "i")
  override def toString = {s"$real1 + $unreal i"
  if (unreal == 0) s"$real1"
  else if (unreal <0) s"$real1 $unreal i"
  else s"$real1 + $unreal i"
  }

  def real(): Double = real1
  def imaginary(): Double = unreal

  def this(n: Double) = this(n,0)

  def +(sec: Complex): Complex =
    new Complex(real1 + sec.real1, unreal + sec.unreal)

  def +(sec: Double): Complex =
    new Complex(real1+sec, unreal)

  def -(sec: Complex): Complex =
    new Complex(real1 - sec.real1, unreal - sec.unreal)

  def *(sec: Complex): Complex =
    new Complex(real1*sec.real1 - unreal*sec.unreal, real1*sec.unreal + unreal*sec.real1)

  def *(sec: Double): Complex =
    new Complex(real1*sec, unreal*sec)

  def conjugate(): Complex =
    new Complex(real1, - unreal)

  def /(sec: Complex): Complex =
    {
      var num1 = this*(sec.conjugate)
      var num = sec*(sec.conjugate)
      var num2 = num.real1
      new Complex(num1.real1/num2, num1.unreal/num2)
    }

  def reciprocal(): Complex = {
    var num = this*(this.conjugate)
    var num2 = num.real
    new Complex(this.real/num2, this.unreal/num2)
  }

}
