type parseResult<'a> =
  | Success('a)
  | Failure(string)

type parser<'a> = Parser(string => parseResult<'a>)
let pchar = charToMatch => {
  let innerFn = str => {
    if str == "" {
      Failure("No more input")
    } else {
      let first = Js.String2.get(str, 0)
      if first == charToMatch {
        let remaining = Js.String.sliceToEnd(~from=1, str)
        Success(charToMatch, remaining)
      } else {
        let msg = `Expecting ${charToMatch}. Got ${first}`
        Failure(msg)
      }
    }
  }
  Parser(innerFn)
}

let run = (parser, input) => {
  let Parser(innerFn) = parser
  innerFn(input)
}

let andThen = (parser1, parser2) => {
  let innerFn = input => {
    let result1 = run(parser1, input)
    switch result1 {
    | Failure(err) => Failure(err)
    | Success(value1, remaining1) => {
        let result2 = run(parser2, remaining1)

        switch result2 {
        | Failure(err) => Failure(err)
        | Success(value2, remaining2) => {
            let newValue = (value1, value2)
            Success(newValue, remaining2)
          }
        }
      }
    }
  }
  Parser(innerFn)
}

let orElse = (parser1, parser2) => {
  let innerFn = input => {
    let result1 = run(parser1, input)
    switch result1 {
    | Success(_) => result1
    | Failure(_) => {
        let result2 = run(parser2, input)
        result2
      }
    }
  }
  Parser(innerFn)
}

let choice = listOfParsers => Js.Array.reduce(orElse, listOfParsers[0], listOfParsers)

let anyOf = listOfChars => listOfChars |> Js.Array.map(pchar) |> choice
