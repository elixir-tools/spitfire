# Spitfire

Spitfire is a new error resilient parser for Elixir.

The goal of the project is to make it easier for editor tooling to provide code intelligence for incomplete snippets of Elixir code.

## Progress

- [x] Parse all Elixir Syntax into core compatible AST.
- [x] Error resilience
- [x] Environment querying API

## Design

The project is a handwritten Pratt parser (a type of recursive descent parser). A hand written parser allows for better error tolerance as well as (hopefully) better error messages.

Pratt parsers are becoming very popular in the realm of error tolerant parsing. Projects that I have been watching and learning from include the `rustc` parser as well as the new [Prism](https://github.com/ruby/prism) parser for the Ruby project.

## Contributing

With the parser complete, the goal is now to improve error resilience, parsing speed, and improve the environment querying capabilities.

## Acknowledgements

I'd like to thank Thorsten Ball, the author of [Writing an Interpreter in Go](https://interpreterbook.com). I read this book a couple of years ago and built out the language from the book. Thorsten teaches you how to build a Pratt parser, which is really proving helpful for the development of Spitfire.

## License

MIT License

Copyright (c) 2023 Mitchell Hanberg

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
