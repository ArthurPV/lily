let main_help =
  {|Usage: lily [command] [options]

Options:

    -h, --help          Print lily's help
    -v, --version       Print lily's version

Commands:

    build               Build project
    compile             Compile file
    help                Print lily's help
    init                Init project
    new                 New project
    repl                Start REPL
    run                 Run file (bytecode)
    test                Test file
    to                  Transpile file in Js, Ml, Rb,...
    version             Print lily's version
|}

let run_help = {|Usage: lily run [options] <INPUT>

Options:

    -h, --help          Print run help
|}
