import scala.annotation.tailrec

abstract class ArbolHuffman {

  type Bit = 0 | 1
  def peso: Int = this match
    case HojaHuffman(n, p) => p
    case RamaHuffman(izq, dch) => izq.peso + dch.peso


  def caracteres: List[Char] = this match
      case HojaHuffman(letra, peso) => List(letra)
      case RamaHuffman(izq, dch) => izq.caracteres ++ dch.caracteres

  /*def saber(bits: List[Bit], cadena: String): String = this match
    case HojaHuffman(letra, peso) => (cadena + letra,bits.tail)
    case RamaHuffman(izq,dch) if (bits.head == 1) => dch.saber(bits.tail, cadena + bits.head)
    case RamaHuffman(izq, dch) if (bits.head == 0) => izq.saber(bits.tail, cadena + bits.head)

  def decodificar(bits: List[Bit]): String =
    @tailrec
    def dAux(bits: List[Bit], cadena: String, arbolHuffman: ArbolHuffman): String =
      while(bits != null){
        if (arbolHuffman == HojaHuffman(letra, peso)) => dAux(bits.tailcadena + letra
        case RamaHuffman(izq, dch) if (bits.head == 1) => dch.saber(bits.tail, cadena + bits.head)
        case RamaHuffman(izq, dch) if (bits.head == 0) => izq.saber(bits.tail, cadena + bits.head)

      }
      dAux(bits,"")*/

  def decodificar(bits: List[Bit]): String =
    @tailrec
    def dAux(bits: List[Bit], arbolHuffman: ArbolHuffman, cadenatemp: String): String = (bits, arbolHuffman) match
      case (Nil, _) => cadenatemp
      case (0 :: tail, RamaHuffman(izq, _)) => dAux(tail, izq, cadenatemp)
      case (1 :: tail, RamaHuffman(_, dcha)) => dAux(tail, dcha, cadenatemp)
      case (listabits, HojaHuffman(letra, n)) => dAux(listabits, this, cadenatemp + letra)

    dAux(bits, this, "")
}
def cadenaAListaChars(cadena: String): List [Char] =
  @tailrec
  def cAux(cadena: String, l: List[Char]): List[Char] = cadena match
    case "" => l
    case _ => cAux(cadena.tail, l :+ cadena.head)

  cAux(cadena,List())

def listaCharsACadena(listaCar: List[Char]): String =
  val lista: List[Char] = listaCar.reverse
  @tailrec
  def lAux(listaCar: List[Char], cadena: String): String = listaCar match
    case Nil => cadena
    case _ => lAux(listaCar.tail, listaCar.head.toString + cadena)

  lAux(lista, "")

case class RamaHuffman(nodoizq: ArbolHuffman, nododch: ArbolHuffman) extends ArbolHuffman{

}

case class HojaHuffman(letra: Char, p: Int) extends ArbolHuffman{

}

@main
def main():Unit =
  val Hojas = HojaHuffman('s',4)
  val Hojao = HojaHuffman('o',3)
  val Hojae = HojaHuffman('e',2)
  val Hoja = HojaHuffman(' ', 2)
  val Rama2 = RamaHuffman(Hojae, Hoja)
  val Rama1 = RamaHuffman(Hojao, Rama2)
  val arbolHuffman = RamaHuffman(Hojas, Rama1)
  println(arbolHuffman.peso)
  println(Hojas.peso)
  println(arbolHuffman.caracteres)
  println(Hojas.caracteres)
  println(cadenaAListaChars("Hola me llamo"))
  println(listaCharsACadena(List('h',' ', 'm', 'e')))
