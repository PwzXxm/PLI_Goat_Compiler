proc drawA (val int n)
    int i;
    int j;
begin
    i := 0;
    while i < n do
        j := 0;
        while j <= (n / 2) do
            if ((((j = 0) || (j = (n / 2))) && (i != 0)) || (((i = 0) && (j != 0)) && (j != (n / 2)))) || (i = (n / 2)) then
                write "*";
            else
                write " ";
            fi
            j := j + 1;
        od
        write "\n";
        i := i + 1;
    od
end

proc main ()
begin
    call drawA(5);
    call drawA(10);
end
