import scala.annotation.tailrec

type Bit = 0 | 1
type TablaCodigos = List[(Char, List[Bit])]
abstract class ArbolHuffman {

  // Devuelve el peso de un ArbolHuf.
  def peso: Int = this match
    case HojaHuffman(n, p) => p // te devuelve el peso de la hoja
    case RamaHuffman(izq, dch) => izq.peso + dch.peso // te suma el peso de la rama de la izquierda y de la rama de la derecha

  // Devuelve la lista de caracteres de un ArbolHuf
  def caracteres: List[Char] = this match
      case HojaHuffman(letra, peso) => List(letra)
      case RamaHuffman(izq, dch) => izq.caracteres ++ dch.caracteres

  // Devuelve la cadena que se forma con la lista de bits que se ha metido de bits
  def decodificar(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(bits: List[Bit], arbol: ArbolHuffman, cadenatemp: String): String = (bits, arbol) match
      case (Nil, HojaHuffman(letra, n)) => cadenatemp + letra
      case (Nil, _) => cadenatemp
      case (0 :: tail, RamaHuffman(izq, _)) => decodificarAux(tail, izq, cadenatemp)
      case (1 :: tail, RamaHuffman(_, dcha)) => decodificarAux(tail, dcha, cadenatemp)
      case (listabits, HojaHuffman(letra, n)) => decodificarAux(listabits, this, cadenatemp + letra)

    decodificarAux(bits, this, "")

  // Te dice si el carácter que estás buscando está en el árbol
  def contiene(caracter: Char): Boolean =
    def contieneAux(arbol: ArbolHuffman, caracter: Char): Boolean = arbol match
      case HojaHuffman(letra, p) => letra == caracter
      case RamaHuffman(nodoizq, nododch) => contieneAux(nodoizq, caracter) || contieneAux(nododch, caracter)
    contieneAux(this, caracter)

  // Devuelve la lista de bits q se forma con el string que metes
  def codificar(cadena: String): List[Bit]=
    val listachar: List[Char] = cadenaAListaChars(cadena)
    @tailrec
    def codificarAux(listachar: List[Char], listtemp: List[Bit], arbol: ArbolHuffman): List[Bit] =  (listachar, arbol) match
      case (Nil, _) => listtemp.reverse
      case (head::tail, HojaHuffman(letra, _)) if (head == letra)=> codificarAux(tail, listtemp, this)
      case (head::tail, RamaHuffman(nodoizq, nododch)) if nodoizq.contiene(head) => codificarAux(listachar, 0::listtemp, nodoizq)
      case (head::tail, RamaHuffman(nodoizq, nododch)) if nododch.contiene(head) => codificarAux(listachar, 1::listtemp, nododch)
      case (_,_) => throw new Error("Algún caracter de tu cadena no está en el árbol")
    codificarAux(listachar, List(), this)
  
}

// Transforma la cadena de texto que le das a una lista de carácteres
def cadenaAListaChars(cadena: String): List [Char] =
  @tailrec
  def cAux(cadena: String, l: List[Char]): List[Char] = cadena match
    case "" => l
    case _ => cAux(cadena.tail, l :+ cadena.head)

  cAux(cadena,List())

// Transforma una lista de carácteres en una cadena de texto
def listaCharsACadena(listaCar: List[Char]): String =
  val lista: List[Char] = listaCar.reverse
  @tailrec
  def lAux(listaCar: List[Char], cadena: String): String = listaCar match
    case Nil => cadena
    case _ => lAux(listaCar.tail, listaCar.head.toString + cadena)

  lAux(lista, "")


// Convierte una lista de caracteres en su distribución de frecuencias.
def listaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
  //saber cuantas veces está un caracter dentro de una cadena
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
      val tupla: (Char,Int) = (listaChar.head,contar(listaChar,listaChar.head)) // te produce una tupla con el carácter y las veces que aparece
      val listatemp: List[(Char,Int)]= List(tupla) // te hace una lista con las tuplas
      lAux(listaChar.filter( _ != listaChar.head), lista ++ listatemp)
  lAux(listaChar,List())


// Convierte la distribución en una lista de hojas ordenada
def DistribFrecAListaHojas(frec:List[(Char, Int)]): List[HojaHuffman] =
  @tailrec
  def DistribFrecAListaHojasAux(frec:List[(Char, Int)], listtemp:List[HojaHuffman]): List[HojaHuffman] = frec match
    case Nil => ordenar(listtemp)
    case (letra, peso)::tail => DistribFrecAListaHojasAux(tail, HojaHuffman(letra, peso)::listtemp)
  DistribFrecAListaHojasAux(frec, List())

//ordena la lista de hojas que le das por pesos
def ordenar(lista: List[HojaHuffman]): List[HojaHuffman] =
  //Busca dentro de la lista que le das los pesos para poner el carácter en su sitio
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


// Crea el ArbolHuffman con la ayuda de otras funciones
def crearArbolHuffman(cadena: String): ArbolHuffman =
  val listaHojas: List[HojaHuffman] = DistribFrecAListaHojas(listaCharsADistFrec(cadenaAListaChars(cadena)))
  repetirHasta(combinar, esListaSingleton)(listaHojas)


// Crea un objeto RamaHuff integrando los dos ArbolHuff (izquierdo y derecho) que se le pasan como parámetros
def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuffman =
  RamaHuffman(izq, dch)

//te devuelve verdadero si la lista es igual a uno
def esListaSingleton(lista: List[ArbolHuffman]): Boolean =
  lista.length == 1

//te va creando las ramas en función del peso que tienen las hojas
def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
  case izq::dcha::tail => (creaRamaHuff(izq, dcha)::tail)
  case _ => ordenarpeso(nodos)
  //te deja la lista ordenada por pesos para poder seguir creando ramas siguendo las normas del ArbolHuffman
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


case class RamaHuffman(nodoizq: ArbolHuffman, nododch: ArbolHuffman) extends ArbolHuffman{

}

case class HojaHuffman(letra: Char, p: Int) extends ArbolHuffman{

}

object ArbolHuffman
//constructor para clases abstractas

  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)

  // Transforma un árbol en una lista de tuplas con sus carácteres y sus bits
  def deArbolATabla(arbol: ArbolHuffman): TablaCodigos =
    def deArbolATablaAux(arbol: ArbolHuffman, listabits: List[Bit]): TablaCodigos = arbol match
      case HojaHuffman(letra,_) => List((letra, listabits.reverse))
      case RamaHuffman(izq, dcha) => deArbolATablaAux(izq, 0::listabits)++deArbolATablaAux(dcha, 1::listabits)
    deArbolATablaAux(arbol,List())

  // Transforma un string en un lista de bits con la ayuda de una lista de tuplas con sus carácteres y sus bits
  def codificar(tabla: TablaCodigos)(cadena: String): List[Bit]=
    val listachars = cadenaAListaChars(cadena)
    def deCharABits(caracter: Char): List[Bit] =
      @tailrec
      def deCharABitsAux(tabla: TablaCodigos, caracter: Char): List[Bit] = tabla match
        case (char, listabits)::_ if char == caracter => listabits.reverse
        case _::tail => deCharABitsAux(tail,caracter)
        case _ => throw new Error("El caracter no esta en la tabla")
      deCharABitsAux(tabla, caracter)
    @tailrec
    def codificarAux(tabla: TablaCodigos, listabits: List[Bit])(cadena: List[Char]): List[Bit]= cadena match
      case Nil => listabits.reverse
      case head::tail => codificarAux(tabla,deCharABits(head)++listabits)(tail)
    codificarAux(tabla,List())(listachars)

  // Transforma una lista de bits en un string con la ayuda de una lista de tuplas con sus carácteres y su frecuencia
  def decodificar(tabla: TablaCodigos)(listabits: List[Bit]): String =
    //te cambia la lista de bits por la lista de tuplas con carácteres y la lista bits usados
    def deBitsAChar(listabits: List[Bit]): (Char, List[Bit]) =
      def contienebits(lbitstabla: List[Bit], lbits: List[Bit]): Boolean = (lbitstabla, lbits) match
        case (Nil, _) => true
        case (_, Nil) => false
        case (ltablahead::ltablatail, lbitshead::lbitstail) if ltablahead == lbitshead => contienebits(ltablatail, lbitstail)
        case _ => false
      @tailrec
      def deBitsACharAux(tablaCodigos: TablaCodigos): (Char, List[Bit]) = tablaCodigos match
        case (char, bits)::tail if contienebits(bits, listabits) => (char,bits)
        case _::tail => deBitsACharAux(tail)
        case Nil => throw new NoSuchElementException("No existe")

      deBitsACharAux(tabla)
    // quita los bits que ya se han usado
    def quitarbits(lbits1: List[Bit], lbits2: List[Bit]): List[Bit] =
       @tailrec
       def quitarbitsAux(lbits1: List[Bit], lbits2: List[Bit]): List[Bit] = (lbits1, lbits2) match
         case (Nil,_) => lbits2
         case (_,Nil) => Nil
         case (head1::tail1, head2::tail2) if (head1 == head2) => quitarbitsAux(tail1,tail2)
         case _ => lbits2
       quitarbitsAux(lbits1,lbits2)

    @tailrec
    def decodificarAux(cadenatemp: String, listatemp: List[Bit]): String = listatemp match
      case Nil => cadenatemp
      case _ =>
        val (char, bitsusados) = deBitsAChar(listatemp)
        decodificarAux(cadenatemp + char, quitarbits(bitsusados, listatemp))

    decodificarAux("",listabits)



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
  println(decodificar(tabla)(List(0,1,0,0,1,1,1,1,1,1,0,0,1,0)))
  val miarbol = crearArbolHuffman("this is an example of a huffman tree")
  println(deArbolATabla(miarbol))








