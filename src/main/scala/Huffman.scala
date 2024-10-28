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
    def decodificarAux(bits: List[Bit], arbol: ArbolHuffman, cadenatemp: String): String = (bits, arbol) match
      case (Nil, _) => cadenatemp
      case (0 :: tail, RamaHuffman(izq, _)) => decodificarAux(tail, izq, cadenatemp)
      case (1 :: tail, RamaHuffman(_, dcha)) => decodificarAux(tail, dcha, cadenatemp)
      case (listabits, HojaHuffman(letra, n)) => decodificarAux(listabits, this, cadenatemp + letra)

    decodificarAux(bits, this, "")

  def contiene(caracter: Char): Boolean =
    def contieneAux(arbol: ArbolHuffman, caracter: Char): Boolean = arbol match
      case HojaHuffman(letra, p) => letra == caracter
      case RamaHuffman(nodoizq, nododch) => contieneAux(nodoizq, caracter) || contieneAux(nododch, caracter)
    contieneAux(this, caracter)

  def codificar(cadena: String): List[Bit]=
    val listachar: List[Char] = cadenaAListaChars(cadena)
    @tailrec
    def codificarAux(listachar: List[Char], listtemp: List[Bit], arbol: ArbolHuffman): List[Bit] = (listachar, arbol) match
      case (Nil, _) => listtemp.reverse
      case (head::tail, HojaHuffman(letra, _)) if (head == letra)=> codificarAux(tail, listtemp, this)
      case (head::tail, RamaHuffman(nodoizq, nododch)) if nodoizq.contiene(head) => codificarAux(listachar, 0::listtemp, nodoizq)
      case (head::tail, RamaHuffman(nodoizq, nododch)) if nododch.contiene(head) => codificarAux(listachar, 1::listtemp, nododch)
    codificarAux(listachar, List(), this)

  def listaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
    @tailrec
    def lAux(listaChar: List[Char], lista: List[(Char,Int)]): List[(Char,Int)] = listaChar match
      case Nil => lista
      case _ =>
        val tupla: (Char,Int) = (listaChar.head,contar(listaChar,listaChar.head))
        val listatemp: List[(Char,Int)]= List(tupla)
        lAux(quitar(listaChar,listaChar.head), lista ++ listatemp)
    lAux(listaChar,List())
}

def contar(lista: List[Char], char: Char): Int = //saber cuantas veces está un caracter dentro de una frase

  @tailrec
  def cAux(lista: List[Char], n: Int, char: Char): Int = lista match
    case Nil => n
    case _ if (lista.head == char) => cAux(lista.tail, n + 1, char)
    case _ => cAux(lista.tail, n, char)

  cAux(lista, 0, char)

def quitar(lista: List[Char], char: Char): List[Char] = // devuelve la lista sin el carácter que ya se ha buscado
  @tailrec
  def qAux(lista: List[Char], char: Char, listatemp: List[Char]): List[Char] = lista match
    case Nil => listatemp
    case _ if ((lista.head == char) && (listatemp !=Nil)) => qAux(lista.tail, char,listatemp.init ++ lista.tail)
    case _ if ((lista.head == char) && (listatemp !=Nil)) => qAux(lista.tail, char,listatemp ++ lista.tail)
    case _ => qAux(lista.tail, char, listatemp)

  qAux(lista, char, List())
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
  println(arbolHuffman.decodificar(List(0,1,0,0,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,0)))
  println(arbolHuffman.contiene('s'))
  println(arbolHuffman.contiene('p'))
  println(arbolHuffman.codificar("sos "))
  println(arbolHuffman.listaCharsADistFrec(List('h',' ', 'm', 'e', 'h')))
