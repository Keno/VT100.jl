using VT100
using Base.Test

failed_tests = 0
for test in [
    "basic/hello", "basic/hello-nocr", "basic/long",
    "basic/color", "basic/boserase"
]
    em = ScreenEmulator(80,24)
    basepath = joinpath(dirname(@__FILE__),test)
    if isfile(string(basepath,".raw.in"))
        open(f->(parseall!(em,f)),string(basepath,".raw.in"))
    else
        @assert isfile(string(basepath,".in"))
        open(f->(parseall!(em,IOBuffer(unescape_string(readstring(f))))),
            string(basepath,".in"))
    end
    output = open(read,string(basepath,".out"))
    buf = IOBuffer()
    VT100.dump(buf,DevNull,em)
    outbuf = take!(buf)
    if outbuf != output
        print_with_color(:red, "Failed test $test\n")
        failed_tests += 1
        println(output)
        println(outbuf)
        println(String(output))
        println(String(outbuf))
    end
end
exit(failed_tests)
