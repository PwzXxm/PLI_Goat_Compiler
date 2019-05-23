proc p (ref int i)
begin
    i := i - 1;
end

proc main ()
    int a;
    int b;
begin
    a := 100;
    if a > 0 then
        a := a + 1;
        b := 2 * a;
    else
        a := 2 * a;
        b := 1 - a;
    fi
    while a < b do
        a := a + 1;
        b := a + 30;
        while b > 30 do
            call p(b);
        od
    od
end
