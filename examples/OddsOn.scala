
object OddsOn {

  //--------------------------------------------------------------
  //       Odds-On Game
  //--------------------------------------------------------------

  def getNum(lBound: Int, uBound: Int): Int = {
    Std.printString(""); Std.printString("Here comes the moment, on one, two, three we both click enter and send our number between " ++ Std.intToString(lBound) ++ " and " ++ Std.intToString(uBound) ++ "!");
    Std.readInt()
  }

  def getBound(name: String): Int = {
    Std.printString("So " ++ name ++ ", how low are you willing to set your chances [1,100]?" );
    Std.readInt()
  }

  def getFear(): String = {
    Std.printString("What is it that you are unsure of doing?");
    Std.readString()
  }

  def getName(): String = {
    Std.printString(""); Std.printString("Cool, what is your name?");
    Std.readString()
  }

  //--------------------------------------------------------------
  //       miserable randomizer
  //--------------------------------------------------------------

  def addRandoms(): L.List = {
    Std.printString(""); Std.printString("Please now add some random numbers!");
    val randomNL: L.List = L.Cons(Std.readInt()+101, L.Cons(Std.readInt()+8, L.Cons(Std.readInt(), L.Cons(Std.readInt(), L.Nil()))));
    L.mergeSort(randomNL)
  }

  def rand(list: L.List, uBound: Int): Int = {
    val sum: Int = L.sum(list);
    if (200 < sum) {
      val nL1: L.List = L.concat(L.Cons(L.head(list) + 8, L.Nil()), list);
      val newList: L.List = L.reverse(nL1);
      (L.head(newList) % uBound) + 1
    } else {
      val newList2: L.List = L.reverse(L.take(list, 3));
      (L.head(newList2) % uBound) + 1
    }
  }

  //--------------------------------------------------------------
  //       game controls
  //--------------------------------------------------------------

  def startGame(): Boolean = {
    Std.printString("Let's play a game of Odds-On, shall we?");
    Std.printString("(y: 1/n: 0)");
    val decison: Int = Std.readInt(); // Std.printInt("DEBUG: " ++ decison);
    if(decison == 1) { true } else { false }
  }

  def endGame(name: String): String = {
    "Thanks for playing, " ++ name ++ "!"
  }

  //--------------------------------------------------------------
  //       The Games
  //--------------------------------------------------------------

  if (startGame()) {

    val name: String = getName();
    val uBound: Int = getBound(name);
    val lBound: Int = 1;

    val fear: String = getFear();

    val list: L.List = addRandoms();

    val num: Int = getNum(lBound, uBound);


    if(uBound < num){
      Std.printString("You are disqualified because you are out of range!");
      Std.printString("Au revoir!")
    } else {
      // call random
      val rand: Int = rand(list, uBound);
//      Std.printString("DEBUG: random was: " ++ Std.intToString(rand));
      if(num == rand){
        Std.printString("I've got the same! I dare you, to " ++ fear ++ " now.");
        Std.printString(endGame(name))
      } else {
        Std.printString("Close call but this time you were saved!");
        Std.printString(endGame(name)) //Std.printString("You have managed to escape!");
      }
    }
  } else {
    Std.printString("You are a boring!")
  }

}
