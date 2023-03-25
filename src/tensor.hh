#ifndef SRC_TENSOR_HH
#define SRC_TENSOR_HH

#include <string>
#include <map>
#include <cassert>
#include <regex>

class TensorMode {
 public:
  bool batched;             // true if tensor has batch dimension
  std::string torch_dtype;  // torch datatype of the Tensor
  std::string cpp_dtype;    // C++ analog of the torch datatype
  int n_dims;               // number of dimensions (including batch dimension)

  TensorMode() :
    batched(false), torch_dtype("torch::kFloat32"),
    cpp_dtype("float"), n_dims(2) {}

  TensorMode(bool batched, std::string &torch_dtype,
             std::string &cpp_dtype, int n_dims) :
             batched(batched), torch_dtype(torch_dtype),
             cpp_dtype(cpp_dtype), n_dims(n_dims) {}

  static bool is_tensor(const std::string &input) {
    return input.find("tensor") != input.npos;
  }

  /*
   * Tensor type notation in GAP-L input<> declaration: "["batched"][ND]tensor[dtype]"
   * allowed dtypes: F32 (default), F64, I32, I64
   * max number of dimensions (includes batch dimension (if it exists)): 99 (default: 2)
   * examples:
   *  - input<batchedtensorF32> : batched (3D) tensor containing float32 values
   *  - input<batched4DtensorI32> : batched 4D tensor containing int32 values
   *  - input<tensorF64> : non-batched (2D) tensor containing float64 values
   *  - input<batchedtensor> : batched (2D) tensor containing float32 values
   */
  static TensorMode get_tensor_mode(const std::string &input) {
    assert(is_tensor(input));

    static std::map<std::string, std::pair<std::string, std::string>>
    torch_type = {{"I32", {"torch:kInt32", "int"}},
                  {"I64", {"torch::kInt64", "long"}},
                  {"F32", {"torch:kFloat32", "float"}},
                  {"F64", {"torch::kFloat64", "double"}}};
    
    // check if input Tensor is batched (default: false)
    bool batched = input.find("batched") != input.npos;
    
    // check the number of dims of the input Tensor (default: 2)
    std::regex regex("[a-zA-Z0-9]*([0-9]{1,2})D[a-zA-Z0-9]*");
    std::smatch match;
    std::regex_search(input, match, regex);

    int n_dims;
    if (match.empty()) {
      n_dims = 2;
    } else {
      try {
        n_dims = std::stoi(match[1].str());
      } catch (const std::exception &e) {
        Log::instance()->error("Couldn't convert " +
                               match[1].str() +
                               " to a number.");
        std::exit(1);
      }
    }
    
    // check the datatype of the tensor values
    std::string torch_dtype = "torch::kFloat32";
    std::string cpp_dtype = "float";

    size_t i;
    for (auto& key_val : torch_type) {
      if ((i = input.find(key_val.first)) != input.npos) {
        torch_dtype = key_val.second.first;
        cpp_dtype = key_val.second.second;
        break;
      }
    }
  
    return TensorMode(batched, torch_dtype, cpp_dtype, n_dims);
  }
};

inline bool operator==(const TensorMode &lhs, const TensorMode &rhs) {
  return lhs.batched == rhs.batched &&
         lhs.torch_dtype == rhs.torch_dtype &&
         lhs.n_dims == rhs.n_dims;
}

inline bool operator!=(const TensorMode &lhs, const TensorMode &rhs) {
  return !(lhs == rhs);
}

class TensorInput {
 private:
  std::vector<TensorMode> tensor_modes;

 public:
  const std::vector<TensorMode> &get_modes() const {
    return tensor_modes;
  }

  void add_mode(TensorMode mode) {
    tensor_modes.push_back(mode);
  }
  
  // check if all input Tensors have the same
  // number of dimensions and the same datatype
  bool same() const {
    bool batched, initialized = false;
    std::string dtype;
    int n_dims;

    for (const TensorMode &mode : tensor_modes) {
      if (!initialized) {
        initialized = true;
        batched = mode.batched;
        dtype = mode.torch_dtype;
        n_dims = mode.n_dims;
        continue;
      }
      if (mode.batched != batched ||
          mode.torch_dtype != dtype ||
          mode.n_dims != n_dims) {
        return false;
      }
    }

    return true;
  }
  
  /*
   * get the shared data type (as a C++ type) of all input Tensors;
   * if they don't all share the same type, an empty String is returned
   */
  std::string shared_cpp_dtype() const {
    if (same()) {
      return tensor_modes.front().cpp_dtype;
    } else {
      return std::string("");
    }
  }

  /*
   * get the shared data type (as a torch type) of all input Tensors;
   * if they don't all share the same type, an empty String is returned
   */
  std::string shared_torch_dtype() const {
    if (same()) {
      return tensor_modes.front().torch_dtype;
    } else {
      return std::string("");
    }
  }

  /*
   * get the shared number of dimensions of all input Tensors;
   * if they don't all share the same number of dimensions, 0 is returned
   */
  int shared_ndims() const {
    if (same()) {
      return tensor_modes.front().n_dims;
    } else {
      return 0;
    }
  }
  
  // check if all input Tensors are "batched"
  bool all_batched() const {
    bool batched = true;
    for (const TensorMode &mode : tensor_modes) {
      batched &= mode.batched;
    }

    return batched;
  }
};

#endif  // SRC_TENSOR_HH
