proc main ()
    int input;
    int result;
begin
    write "Provide an integer value: ";
    read input;
    call mccarthy(input, result);
    write "McCarthy's function applied to ";
    write input;
    write " yields ";
    write result;
    write "\n";
end

proc mccarthy (val int in, ref int out)
    int value;
begin
    if in > 100 then
        out := in - 10;
    else
        call mccarthy(in + 11, value);
        call mccarthy(value, out);
    fi
end
