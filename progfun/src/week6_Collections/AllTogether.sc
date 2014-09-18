package week6_Collections

//The class Source implements an iterable representation of source data
import scala.io.Source

object AllTogether {
  //IMP: http://alvinalexander.com/scala/scala-how-to-download-url-contents-to-string-file
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords")
                                                  //> java.nio.charset.MalformedInputException: Input length = 1
                                                  //| 	at java.nio.charset.CoderResult.throwException(CoderResult.java:260)
                                                  //| 	at sun.nio.cs.StreamDecoder.implRead(StreamDecoder.java:319)
                                                  //| 	at sun.nio.cs.StreamDecoder.read(StreamDecoder.java:158)
                                                  //| 	at java.io.InputStreamReader.read(InputStreamReader.java:167)
                                                  //| 	at java.io.BufferedReader.fill(BufferedReader.java:136)
                                                  //| 	at java.io.BufferedReader.read(BufferedReader.java:157)
                                                  //| 	at scala.io.BufferedSource$$anonfun$iter$1$$anonfun$apply$mcI$sp$1.apply
                                                  //| $mcI$sp(BufferedSource.scala:38)
                                                  //| 	at scala.io.Codec.wrap(Codec.scala:68)
                                                  //| 	at scala.io.BufferedSource$$anonfun$iter$1.apply(BufferedSource.scala:38
                                                  //| )
                                                  //| 	at scala.io.BufferedSource$$anonfun$iter$1.apply(BufferedSource.scala:38
                                                  //| )
                                                  //| 	at scala.collection.Iterator$$anon$9.next(Iterator.scala:162)
                                                  //| 	at scala.collection.Iterator$$anon$17.hasNext(Iterator.scala:511)
                                                  //| 	at scala.collection.Iterator$$anon$11.hasNext(Iterator.scala:327)
                                                  //| 	at sc
                                                  //| Output exceeds cutoff limit.

  //IMP convert Iterator to a List and we filter all the words that contains something that is not a letter
  val words = in.getLines().toList filter (word => word forall (chr => chr.isLetter))

  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  /**Invert the nmen to give a map from chars 'A'.... to '2'... */
  val charCode: Map[Char, Char] = //wrong way : Map('A'->'2', 'B' ->)
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit

  /** Maps a word to the digit string it can represent, e.g "Java" -> "5282"*/
  def wordCode(word: String): String =
    word.toUpperCase map charCode

  /**
   * A map from digit strings to the words that represent them,
   * e.g "5282" -> List("Java","Kata","Lava",...)
   * Note : A missing number should mat to the empty set e.g "1111" -> List()
   */
  val wordsForNum: Map[String, Seq[String]] =
    //we want to group, and we add the withDefaultValue to do not get a noSuchElement Exception
    words groupBy wordCode withDefaultValue Seq()

  /** Return all ways to encode a number as a list of words */
  def encode(number: String): Set[List[String]] =
    if (number.isEmpty()) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet
  //IMPORTANT this trasnformation toSet is becasue the scala interpreter will get the next type available that is indexSeq
  //as we start with a range split <- 1 to number.length and the result cannot be represented a as a range

  encode("7225247386")

  def translate(number: String): Set[String] =
    encode(number) map (_ mkString "")

  translate("7225247386")

}