import scala.annotation.tailrec


type Bit = 0 | 1
type TablaCodigos = List[(Char, List[Bit])]
abstract class ArbolHuffman {

  // Devuelve el peso de un ArbolHuf.
  def peso: Int = this match
    case HojaHuffman(n, p) => p
    case RamaHuffman(izq, dch) => izq.peso + dch.peso

  // Devuelve la lista de caracteres de un ArbolHuf
  def caracteres: List[Char] = this match
      case HojaHuffman(letra, peso) => List(letra)
      case RamaHuffman(izq, dch) => izq.caracteres ++ dch.caracteres


  def decodificar(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(bits: List[Bit], arbol: ArbolHuffman, cadenatemp: String): String = (bits, arbol) match
      case (Nil, HojaHuffman(letra, n)) => cadenatemp + letra
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
    def codificarAux(listachar: List[Char], listtemp: List[Bit], arbol: ArbolHuffman): List[Bit] =  (listachar, arbol) match
      case (Nil, _) => listtemp.reverse
      case (head::tail, HojaHuffman(letra, _)) if (head == letra)=> codificarAux(tail, listtemp, this)
      case (head::tail, RamaHuffman(nodoizq, nododch)) if nodoizq.contiene(head) => codificarAux(listachar, 0::listtemp, nodoizq)
      case (head::tail, RamaHuffman(nodoizq, nododch)) if nododch.contiene(head) => codificarAux(listachar, 1::listtemp, nododch)
    codificarAux(listachar, List(), this)
  
}
// Crea un objeto RamaHuff integrando los dos ArbolHuff (izquierdo y
// derecho)que se le pasan como parámetros
def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuffman =
  RamaHuffman(izq, dch)

def esListaSingleton(lista: List[ArbolHuffman]): Boolean =
  lista.length == 1


def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
  case izq::dcha::tail => (creaRamaHuff(izq, dcha)::tail)
  case _ => ordenarpeso(nodos)
  def ordenarpeso(lista: List[ArbolHuffman]): List[ArbolHuffman] =
    @tailrec
    def ordenarpAux(lista: List[ArbolHuffman], acumulador: List[ArbolHuffman]): List[ArbolHuffman] = lista match
      case Nil => acumulador
      case _ if (lista.head.peso > acumulador.head.peso) => ordenarpAux(lista.tail, acumulador :+ lista.head)
      case _ => ordenarpAux(lista.tail, lista.head :: acumulador)

    ordenarpAux(lista.tail, List(lista.head))

def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(listaHojas: List[ArbolHuffman]): ArbolHuffman =
  if esListaSingleton(listaHojas) then listaHojas.head
  else repetirHasta(combinar, esListaSingleton)(combinar(listaHojas))
  
// Convierte la lista de caracteres en distribución de frecuencias.
def listaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =

  //saber cuantas veces está un caracter dentro de una frase
  def contar(lista: List[Char], char: Char): Int =
    @tailrec
    def cAux(lista: List[Char], n: Int, char: Char): Int = lista match
      case Nil => n
      case _ if (lista.head == char) => cAux(lista.tail, n + 1, char)
      case _ => cAux(lista.tail, n, char)

    cAux(lista, 0, char)
  @tailrec
  def lAux(listaChar: List[Char], lista: List[(Char,Int)]): List[(Char,Int)] = listaChar match
    case Nil => lista
    case _ =>
      val tupla: (Char,Int) = (listaChar.head,contar(listaChar,listaChar.head))
      val listatemp: List[(Char,Int)]= List(tupla)
      lAux(listaChar.filter( _ != listaChar.head), lista ++ listatemp)
      //El filter lo hemos dado en teoria asi que supongo que se podra usar
  lAux(listaChar,List())


// Convierte la distribución en una lista de hojas ordenada
def DistribFrecAListaHojas(frec:List[(Char, Int)]): List[HojaHuffman] =
  @tailrec
  def DistribFrecAListaHojasAux(frec:List[(Char, Int)], listtemp:List[HojaHuffman]): List[HojaHuffman] = frec match
    case Nil => ordenar(listtemp)
    case (letra, peso)::tail => DistribFrecAListaHojasAux(tail, HojaHuffman(letra, peso)::listtemp)
  DistribFrecAListaHojasAux(frec, List())

def ordenar(lista: List[HojaHuffman]): List[HojaHuffman] =
  
  def insertar(h: HojaHuffman, lista: List[HojaHuffman], listtemp: List[HojaHuffman]): List[HojaHuffman] = lista match
    case Nil => listtemp
    case _ if (h.peso < lista.head.peso) =>
      val l: List[HojaHuffman] = listtemp :+ h
      l ++ lista
    case _ => insertar(h, lista.tail, listtemp :+ lista.head)
  @tailrec
  def ordenarAux(lista: List[HojaHuffman], listtemp: List[HojaHuffman]): List[HojaHuffman] = lista match
    case Nil => listtemp
    case _ if listtemp == Nil => ordenarAux(lista.tail, List(lista.head))
    case _ if (lista.head.peso <= listtemp.head.peso) => ordenarAux(lista.tail, lista.head :: listtemp)
    case _ if (lista.head.peso >= listtemp.last.peso) => ordenarAux(lista.tail, listtemp :+ lista.head)
    case _ => ordenarAux(lista.tail, insertar(lista.head, listtemp, List()))

  ordenarAux(lista, List())
  

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

def crearArbolHuffman(cadena: String): ArbolHuffman =
  val listaHojas: List[HojaHuffman] = DistribFrecAListaHojas(listaCharsADistFrec(cadenaAListaChars(cadena)))
  repetirHasta(combinar, esListaSingleton)(listaHojas)
  

object ArbolHuffman
//constructor para clases abstractas
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
  def deArbolATabla(arbol: ArbolHuffman): TablaCodigos =
    def deArbolATablaAux(arbol: ArbolHuffman, listabits: List[Bit]): TablaCodigos = arbol match
      case HojaHuffman(letra,_) => List((letra, listabits.reverse))
      case RamaHuffman(izq, dcha) => deArbolATablaAux(izq, 0::listabits)++deArbolATablaAux(dcha, 1::listabits)
    deArbolATablaAux(arbol,List())

  def codificar(arbol: TablaCodigos)(cadena: String): List[Bit]=
    val listachars = cadenaAListaChars(cadena)
    def contiene(caracter: Char): List[Bit] =
      def contieneAux(arbol: TablaCodigos, caracter: Char): List[Bit] = arbol match
        case (char, listabits)::_ if char==caracter => listabits.reverse
        case (char, listabits)::tail if char != caracter => contieneAux(tail,caracter)
        case Nil => Nil
      contieneAux(arbol, caracter)

    def codificarAux (arbol: TablaCodigos, listabits: List[Bit])(cadena: List[Char]): List[Bit]= cadena match
      case Nil => listabits.reverse
      case head::tail => codificarAux(arbol,contiene(head)++listabits)(tail)
    codificarAux(arbol,List())(listachars)


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
  println(arbolHuffman.decodificar(List(0,1,0,0,1,1,1,1,1,0,0,1,0)))
  println(arbolHuffman.contiene('s'))
  println(arbolHuffman.contiene('p'))
  println(arbolHuffman.codificar("sos eso"))
  println(listaCharsADistFrec(List('h',' ',' ','m','h', 'e', 'h')))
  println(DistribFrecAListaHojas(List(('h',5), (' ',3), ('m',1), ('e',2), ('t',4))))
  println(esListaSingleton(List(arbolHuffman)))
  println(esListaSingleton(List(arbolHuffman, arbolHuffman)))
  val listaHojas = List(HojaHuffman('s',4), HojaHuffman('o',3), HojaHuffman('e',2), HojaHuffman(' ', 2))
  println(repetirHasta(combinar, esListaSingleton)(ordenar(listaHojas)))
  println(crearArbolHuffman("this is an example of a huffman tree"))
  println(deArbolATabla(arbolHuffman))
  val tabla = deArbolATabla(arbolHuffman)
  val mensaje = "sos eso"
  println(codificar(tabla)(mensaje))



