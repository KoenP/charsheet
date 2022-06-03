export function nub<Type>(arr: Type[]): Type[] {
    return arr.filter((e, i, a) => e !== a[i - 1])
}

export function sortNumbers(arr: number[]) : number[] {
    return arr.sort((a,b) => a-b)
}

type Key = string | number | symbol
export function groupBy<Elem> (getKey: (arg: Elem) => Key, arr: Elem[])
    : { [key: Key]: Elem[] }
{
    const map: { [key: Key]: Elem[] } = {}
    for (const elem of arr) {
        const key: Key = getKey(elem)
        if (map[key] === undefined) {
            map[key] = [elem]
        } else {
            map[key].push(elem)
        }
    }
    return map
}