for size in 100; do
    for period in 1; do
        for tolerance in 1 2 3 4 5 6 7 8 9 10 11 12; do
            ./main.native -size $size -g -t $period -f $tolerance -avg 200
        done;
    done;
done;
