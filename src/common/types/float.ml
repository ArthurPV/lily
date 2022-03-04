type float32 = float
type float64 = float
type float80
type float128

module type Float = sig
  type t

  val zero : t
  val one : t
  val minus_one : t
  val neg : t -> t

  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val fma : t -> t -> t -> t
  val rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val infinity : t
  val neg_infinity : t
  val pi : t
  val max_float : t
  val min_float : t
  val epsilon : t
  val is_finite : t -> bool
  val is_infinite : t -> bool
  val is_nan : t -> bool
  val is_integer : t -> bool
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val of_string_opt : string -> t option
  val to_string : t -> string
  val classify_float : t -> fpclass
  val pow : t -> t -> t
  val sqrt : t -> t
  val cbrt : t -> t
  val exp : t -> t
  val exp2 : t -> t
  val log : t -> t
  val log10 : t -> t
  val log2 : t -> t
  val expm1 : t -> t
  val log1p : t -> t
  val cos : t -> t
  val sin : t -> t
  val tan : t -> t
  val acos : t -> t
  val asin : t -> t
  val atan : t -> t
  val atan2 : t -> t
  val hypot : t -> t -> t
  val cosh : t -> t
  val sinh : t -> t
  val tanh : t -> t
  val acosh : t -> t
  val asinh : t -> t
  val atanh : t -> t
  val erf : t -> t
  val erfc : t -> t
  val trunc : t -> t
  val round : t -> t
  val ceil : t -> t
  val floor : t -> t
  val next_after : t -> t
  val copy_sign : t -> t -> t
  val sign_bit : t -> bool
  val frexp : t -> t * int
  val ldexp : t -> int -> t
  val modf : t -> t * t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val min_max : t -> t -> t * t
  val min_num : t -> t -> t
  val max_num : t -> t -> t
  val min_max_num : t -> t -> t * t
  val hash : t -> int
end

module Float128 = struct
  module Base = struct
    type t = float128
    let bits = 128
    let fmt = "ULL"
    let name = "Float128"

    external add : float128 -> float128 -> float128 = "float128_add"
    external sub : float128 -> float128 -> float128 = "float128_sub"
    external mul : float128 -> float128 -> float128 = "float128_mul"
    external div : float128 -> float128 -> float128 = "float128_div"
  end

  include Base

  let ( +. ) x y = add x y
  let ( -. ) x y = sub x y
  let ( *. ) x y = mul x y
  let ( /. ) x y = div x y 
end
