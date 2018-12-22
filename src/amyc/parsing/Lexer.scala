package amyc
package parsing

import utils._
import scala.io.Source
import java.io.File

// The lexer for Amy.
// Transforms an iterator coming from scala.io.Source to a stream of (Char, Position),
// then uses a functional approach to consume the stream.
object Lexer extends Pipeline[List[File], Stream[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "abstract" => Some(ABSTRACT())
    case "Boolean"  => Some(BOOLEAN())
    case "case"     => Some(CASE())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "else"     => Some(ELSE())
    case "error"    => Some(ERROR())
    case "extends"  => Some(EXTENDS())
    case "false"    => Some(FALSE())
    case "if"       => Some(IF())
    case "Int"      => Some(INT())
    case "match"    => Some(MATCH())
    case "object"   => Some(OBJECT())
    case "String"   => Some(STRING())
    case "true"     => Some(TRUE())
    case "Unit"     => Some(UNIT())
    case "val"      => Some(VAL())
    case _          => None
  }

  private def lexFile(ctx: Context)(f: File): Stream[Token] = {
    import ctx.reporter._

    // Special character which represents the end of an input file
    val EndOfFile: Char = scala.Char.MaxValue

    val source = Source.fromFile(f)

    // Useful type alias:
    // The input to the lexer will be a stream of characters,
    // along with their positions in the files
    type Input = (Char, Position)

    def mkPos(i: Int) = Position.fromFile(f, i)

    // The input to the lexer
    val inputStream: Stream[Input] =
      source.toStream.map(c => (c, mkPos(source.pos))) #::: Stream((EndOfFile, mkPos(source.pos)))

    /** Gets rid of whitespaces and comments and calls readToken to get the next token.
      * Returns the first token and the remaining input that did not get consumed
      */
    @scala.annotation.tailrec
    def nextToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      if (Character.isWhitespace(currentChar)) {
        nextToken(stream.dropWhile{ case (c, _) => Character.isWhitespace(c) } )
      } else if (currentChar == '/' && nextChar == '/') {
        nextToken(stream.dropWhile{ case (c, _) => c != '\n' && c != `EndOfFile`}) //drop while no new line and not end of file
      } else if (currentChar == '/' && nextChar == '*') {

        def skip1(st: Stream[Input]): Stream[Input] = st.dropWhile{ case (c, _) => c != '*' && c != `EndOfFile`} //stop at a * to check the char after

        var s = rest //*....
        var err = s.tail.head._1 == `EndOfFile`

        if(!err) {
          do {
            s = skip1(s.tail) //*... || EOF
            err = s.head._1 == `EndOfFile`
          } while (!err && s.tail.head._1 != '/')
        }

        if(err) {
          error("Unclosed block Comment", currentPos)
          nextToken(s)
        } else {
          nextToken(s.tail.tail)
        }
      } else {
        readToken(stream)
      }
    }

    /** Reads the next token from the stream. Assumes no whitespace or comments at the beginning.
      * Returns the first token and the remaining input that did not get consumed.
      */
    def readToken(stream: Stream[Input]): (Token, Stream[Input]) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      // Returns input token with correct position and uses up one character of the stream
      def useOne(t: Token) = (t.setPos(currentPos), rest)
      // Returns input token with correct position and uses up two characters of the stream
      def useTwo(t: Token) = (t.setPos(currentPos), rest.tail)

      currentChar match {
        case `EndOfFile` => useOne(EOF())

        // Reserved word or Identifier
        case _ if Character.isLetter(currentChar) =>
          val (wordLetters, afterWord) = stream.span { case (ch, _) =>
            Character.isLetterOrDigit(ch) || ch == '_'
          }
          val word = wordLetters.map(_._1).mkString
          val kw = keywords(word)

          if (kw.isEmpty) { //None -> no reserved word
            (useOne(ID(word))._1, afterWord)
          } else {
            (useOne(kw.get)._1, afterWord)
          }

        // Int literal
        case _ if Character.isDigit(currentChar) =>
          val (digits, afterDigits) = stream.span { case (d, _) =>
            Character.isDigit(d)
          }
          val number = BigInt(digits.map(_._1).mkString)
          if(!number.isValidInt) { //check if int fits 32 bits
            error("does not fit 32 bits", currentPos)
            (useOne(BAD())._1, afterDigits)
          } else {
            (useOne(INTLIT(number.toInt))._1, afterDigits)
          }
        // String literal
        case '"' =>
          val (stringLetters, afterString) = stream.tail.span {case (c, _) =>
            c != '"' && c != '\n' && c != `EndOfFile` //take all chars except for " and new line
          }
          if (afterString.head._1 == '\n' || afterString.head._1 == `EndOfFile`) { //string literal doesn't have a formal end
            error("Strings has not proper end", currentPos)
            (useOne(BAD())._1, afterString.tail)
          } else {
            val string = stringLetters.map(_._1).mkString
            (useOne(STRINGLIT(string))._1, afterString.tail)
          }
        case ';' => useOne(SEMICOLON())
        case '+' =>
          if (nextChar == '+') {
            useTwo(CONCAT())
          } else {
            useOne(PLUS())
          }
        case '-' => useOne(MINUS())
        case '*' => useOne(TIMES())
        case '/' => useOne(DIV())
        case '%' => useOne(MOD())
        case '<' =>
          if (nextChar == '=') {
            useTwo(LESSEQUALS())
          } else {
            useOne(LESSTHAN())
          }
        case '&' if (nextChar == '&' )=>
          useTwo(AND())
        case '|' if (nextChar == '|') =>
          useTwo(OR())
        case '=' =>
          if (nextChar == '>') {
            useTwo(RARROW())
          } else if (nextChar == '=') {
            useTwo(EQUALS())
          } else {
            useOne(EQSIGN())
          }
        case '!' => useOne(BANG())
        case '{' => useOne(LBRACE())
        case '}' => useOne(RBRACE())
        case '(' => useOne(LPAREN())
        case ')' => useOne(RPAREN())
        case ',' => useOne(COMMA())
        case ':' => useOne(COLON())
        case '.' => useOne(DOT())
        case '_' => useOne(UNDERSCORE())
        case _ =>
          error("Invalid character", currentPos)
          useOne(BAD())
      }
    }

    // To lex a file, call nextToken() until it returns the empty Stream as "rest"
    def tokenStream(s: Stream[Input]): Stream[Token] = {
      if (s.isEmpty) Stream()
      else {
        val (token, rest) = nextToken(s)
        token #:: tokenStream(rest)
      }
    }

    tokenStream(inputStream)
  }

  // Lexing all input files means putting the tokens from each file one after the other
  def run(ctx: Context)(files: List[File]): Stream[Token] = {
    files.toStream flatMap lexFile(ctx)
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Stream[Token], Unit] {
  def run(ctx: Context)(tokens: Stream[Token]): Unit = {
    tokens.toList foreach { t => println(s"$t(${t.position.withoutFile})") }
  }
}