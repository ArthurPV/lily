type argument = {
    argc: int;
    argv: string array;
}

let new_argument = {
    argc = Array.length Sys.argv;
    argv = Sys.argv;
}
