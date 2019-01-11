# AmyLanguage-CBackend

Project Structure before compiler improvement stage:

```
amyc 
├── Main.scala (updated: Lab02, Lab03, Lab04, Lab05, Lab06) 
│ 
├── analyzer 
│ 	├── SymbolTable.scala 
│ 	├── NameAnalyzer.scala          (Lab04)
│ 	└── TypeChecker.scala           (Lab05) 
│ 
├── ast 
│ 	├── Identifier.scala 
│ 	├── Printer.scala 
│ 	└── TreeModule.scala
│
├── codegen/
│   ├── CodeGen.scala               (Lab06)
│   ├── CodePrinter.scala
│   └── Utils.scala
│ 
├── interpreter 
│ 	└── Interpreter.scala           (Lab01)
│ 
├── parsing 
│ 	├── ASTConstructor.scala 
│ 	├── ASTConstructorLL1.scala     (Lab03)
│ 	├── Parser.scala                (Lab03)
│ 	├── Lexer.scala                 (Lab02)
│ 	└── Tokens.scala 
│ 
├── utils/
│   ├── AmycFatalError.scala
│   ├── Context.scala
│   ├── Document.scala
│   ├── Env.scala
│   ├── Pipeline.scala
│   ├── Position.scala
│   ├── Reporter.scala
│   └── UniqueCounter.scala
│
└── wasm/
    ├── Function.scala
    ├── Instructions.scala
    ├── Module.scala
    └── ModulePrinter.scala
```