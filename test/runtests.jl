using VT100
using Base.Test
using Compat

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
    outbuf = takebuf_array(buf)
    if outbuf != output
        print_with_color(:red, "Failed test $test\n")
        failed_tests += 1
        println(output)
        println(outbuf)
        println(Compat.UTF8String(output))
        println(Compat.UTF8String(outbuf))
    end
end
exit(failed_tests)
