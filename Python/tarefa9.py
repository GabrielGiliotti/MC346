def funcao(string1, string2):
    stringOut = ""
    for i in range(len(string1)):
        if(string2.find(string1[i:]) == 0):
            stringOut = string1[i:]
            break

    return stringOut

# Usado para testar
#subString = funcao("abcxxaxxa", "xxaxxaabcd")
#print(subString)
#print(len(subString))