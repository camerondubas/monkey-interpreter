let arrayFunctions = fn() {
    let reduce = fn(arr, initial, f) {
        let iter = fn(arr, result) {
            if (len(arr) == 0) {
                result
            } else {
                if (len(arr) == 1) {
                    f(result, first(arr));
                } else {
                    iter(range(arr, 1, len(arr) - 1), f(result, first(arr)));
                }
            }
        };

        iter(arr, initial);
    };

    let map = fn(arr, f) {
        reduce(arr, [], fn(initial, el) { push(initial, f(el)) });
    };

    let sum = fn(arr) {
        reduce(arr, 0, fn(initial, el) { initial + el });
    };

    let concat = fn(arrOne, arrTwo) {
        let iter = fn(arr, result) {
            if (len(arr) == 0) {
                result
            } else {
                if (len(arr) == 1) {
                    push(result, first(arr));
                } else {
                    iter(range(arr, 1, len(arr) - 1), push(result, first(arr)));
                }
            }
        };

        iter(arrTwo, arrOne);
    };

    return {
        "reduce": reduce,
        "map": map,
        "sum": sum,
        "concat": concat
    };
};


let numberFunctions = fn() {
    let add = fn(x, y) {
        x + y;
    };

    let fib = fn(n) {
        if (n < 2) {
            n
        } else {
            fib(n - 1) + fib(n - 2)
        }
    };


    let fact = fn(n) {
        if (n < 2) {
            1
        } else {
            n * fact(n - 1)
        }
    }

    return {
        "add": add,
        "fib": fib,
        "fact": fact
    };
}

let numFns = numberFunctions();
let arrayFns = arrayFunctions();

let build_range = fn(start, end) {
    if (start > end) {
        [];
    } else {
        push(build_range(start + 1, end), start);
    }
};

if (len("some string") == 11) {
    let arr = [1, 2, 3, 4, 5];
    if (arr[2] == 2) {
        numFns["fact"](5);
    } else {
        numFns["fib"](10);
    }

    let hash = {
        "one": 1,
        "two": 2,
        "three": 3
    };


    if (hash["one"] == false) {
        let arr = [1, 2, 3, 4, 5];
        let arrTwo = arrayFns["map"](arr, fn(x) { x * 2 });
        sum(arrTwo);
    } else {
        let arr = build_range(1, 100);
        let arrTwo = arrayFns["map"](arr, fn(x) { x * 2 });
        arrayFns["sum"](arrTwo);
    }

    let concatStr = fn(strOne, strTwo) {
        strOne + strTwo
    };

    if (arrayFns["concat"]([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]) {
        if (concatStr("Hello ", "World!") == "Hello World!") {
            let arr = [1, 2, 3, 4, 5];
            let arrTwo = arrayFns["map"](arr, fn(x) { x * 2 });
            arrayFns["sum"](arrTwo);
        } else {
            let arr = build_range(1, 100);
            let arrTwo = arrayFns["map"](arr, fn(x) { x * 2 });
            arrayFns["sum"](arrTwo);
        }

    }
}