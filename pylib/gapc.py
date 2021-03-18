import numpy as np

class Basic_Subsequence:
    seq = None

    def __init__(self, sequence:str, i:int, j:int):
        assert i >= 0, "left border i=%i cannot be negative" % i
        assert j >= 0, "right border j=%i cannot be negative" % j
        assert i <= j, "left border i=%i cannot be larger than right border j=%i" % (i,j)
        if sequence is not None:
            assert i <= len(sequence), "left border i=%i cannot be larger then length of sequence \"%s\"" % (j, sequence)
            assert j <= len(sequence), "right border j=%i cannot be larger then length of sequence \"%s\"" % (j, sequence)
        self.seq = sequence
        self.i = i
        self.j = j

    def isEmpty(self):
        return self.seq == None

class REGION(Basic_Subsequence):
    def __init__(self, sequence:str, i:int, j:int):
        assert i < j, "For REGION, left border i=%s must be smaller than right border j=%i" % (i,j)
        super().__init__(sequence, i, j)

class BASE(Basic_Subsequence):
    def __init__(self, sequence:str, i:int, j:int):
        assert i < j, "For BASE, left border i=%s must be smaller than right border j=%i" % (i,j)
        super().__init__(sequence, i, j)

class LOC(Basic_Subsequence):
    def __init__(self, sequence:str, i:int, j:int):
        assert i == j, "For LOC, left border i=%s must be equal to right border j=%i" % (i,j)
        super().__init__(sequence, i, j)


def is_not_empty(x):
    if (type(x) == float) or (type(x) == np.float64) or (type(x) == int):
        return (not np.isnan(x))
    elif (type(x) == LOC) or (type(x) == BASE) or (type(x) == REGION):
        return (not x.isEmpty())
    raise ValueError("is_not_empty not implemented for type %s" % type(x))

def push_back_sum(current_sum:float, value:float) -> float:
    if np.isnan(current_sum):
        return value
    else:
        return current_sum + value

def TUSubsequence(value):
    # an empty function to simply pass through the value.
    # necessary for more advanced ADP stuff, which is not needed
    # in this early stage prototype
    return value
