#!/usr/bin/env python3

import torch, argparse
import numpy as np
import os

SUPPORTED_ALIGNMENT_MODES = ["nw", "gotoh"]

# tests for regular (single) input and batched inputs
# (Truth files are in gapc-test-suite/Truth/Regress)
TESTS = {
        "regular" :
        {
            "nw"    : ((("FREIZEIT", "ZEITGEIST"), "pytorch_mod_test_nw_1.ref"),
                       (("ABCDEF", "AAAAAA"), "pytorch_mod_test_nw_2.ref"),
                       (("MYCOOLSTRING", "EVENCOOLERSTRING"), "pytorch_mod_test_nw_3.ref")),
            "gotoh" : ((("FREIZEIT", "ZEITGEIST"), "pytorch_mod_test_gotoh_1.ref"),
                       (("ABCDEF", "AAAAAA"), "pytorch_mod_test_gotoh_2.ref"),
                       (("MYCOOLSTRING", "EVENCOOLERSTRING"), "pytorch_mod_test_gotoh_3.ref"))
        },

        "batched" :
        {
            "nw"    : (((("AAFREIZEITAA", "ABCDEFGHIJKL", "MYCOOLSTRING"),
                         ("BBZEITGEISTB", "AAAAAAAAAAAA", "COOLERSTRING")), "pytorch_mod_batched_test_nw_1.ref"),),
            "gotoh" : (((("AAFREIZEITAA", "ABCDEFGHIJKL", "MYCOOLSTRING"),
                         ("BBZEITGEISTB", "AAAAAAAAAAAA", "COOLERSTRING")), "pytorch_mod_batched_test_gotoh_1.ref"),),
        }
        }


def encode(seq: str) -> torch.Tensor:
    # encode single sequence string
    # and return it as a 1D Tensor
    # simulate encoder of a protein sequence
    encoder = {c : np.float32(ord(c) - ord("A")) for c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ"}

    return torch.Tensor([encoder[c] for c in seq.upper()])

def batched_encode(seqs) -> torch.Tensor :
    # encode multiple sequence strings of same length
    # and return them as a batched 2D Tensor

    tensor = torch.empty((len(seqs), len(seqs[0])))

    for i, seq in enumerate(seqs):
        tensor[i, :] = encode(seq)
    
    return tensor

def convert_to_tensor(file: str, shape) -> torch.Tensor:
    # read reference matrices from file
    # and convert them to a list of torch.Tensor objects
    reference_matrices = []
    with open(file, "r") as fin:
        for l in fin:
            l = l.strip()
            matrix = torch.Tensor(tuple(map(np.float32, l.split()))).reshape(shape)
            reference_matrices.append(matrix)
    
    return reference_matrices

def almost_equal(bw_matrices, reference_matrices, acceptable_error : float = 1e-5) -> bool:
    # check if all values of all backward matrices
    # are almost equal to their respective reference matrices (see acceptable_error)
    equal: bool = True
    for matrix, reference in zip(bw_matrices, reference_matrices):
        # make sure both matrices have the same dtype (float32)
        # and don't contain any inf vals (bug if so?)
        matrix = matrix.float()
        reference = reference.float()
        matrix[matrix == float("inf")] = 0.0
        reference[reference == float("inf")] = 0.0

        total_vals: int = matrix.numel()
        equal_vals: int = torch.isclose(matrix, reference, acceptable_error).sum()
        equal = equal and (equal_vals == total_vals)

        if not equal:
            print(f"### FAIL ###")
            print(f"{total_vals - equal_vals}/{total_vals} values differ.")
            print(matrix[~torch.isclose(matrix, reference, acceptable_error)])
            print(reference[~torch.isclose(matrix, reference, acceptable_error)])
            print("\n")
    
    return equal

def run_tests(mode: str, batched: bool) -> int:
    # tests correctness of calculated backward matrices of
    # generated pytorch-compatible derivative code

    print(f"Test mode: {mode}")

    if batched:
        encode_input = batched_encode
        tests = TESTS["batched"][mode]
    else:
        encode_input = encode
        tests = TESTS["regular"][mode]
    
    total_tests: int = len(tests)
    success_c: int = 0
    fail_c: int = 0

    for i, ((seq_1, seq_2), ref_file) in enumerate(tests, start=1):
        print(f"### Executing test {i}/{total_tests} ###")

        # encode the input sequence strings
        inp_1: torch.Tensor = encode_input(seq_1)
        inp_2: torch.Tensor = encode_input(seq_2)

        _ = forward(inp_1, inp_2)             # execute forward pass
        bw_matrices = backward(inp_1, inp_2)  # execute backward pass

        bw_shape = bw_matrices[0].shape
        ref_matrix_file = os.path.join(TRUTH_DIR, ref_file)
    
        reference_matrices = convert_to_tensor(ref_matrix_file, bw_shape)

        if almost_equal(bw_matrices, reference_matrices):
            print("### OK ###")
            success_c += 1
        else:
            fail_c += 1
    
    print(f"All tests completed. {fail_c}/{total_tests} tests failed.")
   
    return 1 if fail_c else 0

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", type=str, choices=SUPPORTED_ALIGNMENT_MODES + ["all"],
                        help="aligment mode")
    parser.add_argument("truth_dir", type=str,
                        help="path to directory with Truth matrices")
    parser.add_argument("--batched", "-b", action="store_true",
                        help="run with batched input")
    args = parser.parse_args()
    
    exit_code: int = 0

    mode: str = args.mode
    batched: bool = args.batched
    TRUTH_DIR = args.truth_dir

    if batched:
        if mode == "nw":
            import nw_gapc_batched
            forward = nw_gapc_batched.forward_D1
            backward = nw_gapc_batched.backward_D1
        elif mode == "gotoh":
            import gotoh_gapc_batched
            forward = gotoh_gapc_batched.forward_D1
            backward = gotoh_gapc_batched.backward_D1
    else:
        if mode == "nw":
            import nw_gapc
            forward = nw_gapc.forward_D1
            backward = nw_gapc.backward_D1
        elif mode == "gotoh":
            import gotoh_gapc
            forward = gotoh_gapc.forward_D1
            backward = gotoh_gapc.backward_D1

    if mode == "all":
        for mode in SUPPORTED_ALIGNMENT_MODES:
            exit_code += run_tests(mode, batched)
    else:
        exit_code = run_tests(mode, batched)

    exit(exit_code)
