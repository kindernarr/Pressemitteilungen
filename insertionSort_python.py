def insertionSort(arr): 
    for i in range(1, len(arr)): 
        key = arr[i] 
        j = i-1
        while j >=0 and r.whoisleft(key, arr[j]): 
                arr[j+1] = arr[j] 
                j -= 1
        arr[j+1] = key
    return arr
