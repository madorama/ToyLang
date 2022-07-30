let log = (x) => {
    console.log(x)
    x
}

let each = (f, xs) => {
    ```for(const x of xs) {```
        f(x)
    ```}```
}

let map = (f, xs) => {
    let result = []
    ```for(const x of xs) {```
        result.push(f(x))
    ```}```
    result
}

[1, 2, 3, 4, 5]
    |> map((x) => x * 2)
    |> each(log)
