class Basic_Subsequence:
    def __init__(self, sequence:str, i:int, j:int):
        assert i >= 0, "left border i=%i cannot be negative" % i
        assert j >= 0, "right border j=%i cannot be negative" % j
        assert i <= j, "left border i=%i cannot be larger than right border j=%i" % (i,j)
        assert i <= len(sequence), "left border i=%i cannot be larger then length of sequence \"%s\"" % (j, sequence)
        assert j <= len(sequence), "right border j=%i cannot be larger then length of sequence \"%s\"" % (j, sequence)
        self.seq = sequence
        self.i = i
        self.j = j

class BASE(Basic_Subsequence):
    def __init__(self, sequence:str, i:int, j:int):
        assert i < j, "For BASE, left border i=%s must be smaller than right border j=%i" % (i,j)
        super().__init__(sequence, i, j)

class LOC(Basic_Subsequence):
    def __init__(self, sequence:str, i:int, j:int):
        assert i == j, "For LOC, left border i=%s must be equal to right border j=%i" % (i,j)
        super().__init__(sequence, i, j)
