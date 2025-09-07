package intermediate

import "fmt"

// Data type of an address in the three-address code concept.
// The first 8 bits (0-7) are used for the plain data type, the next bits (8+) are used for modifiers.
const (
	// note: the order of the data types is important, do not change it without updating the code of the '(dataType DataType) Is*' methods
	Untyped    DataType = iota // the address does not have a data type
	Integer64                  // signed 64-bit integer
	Integer32                  // signed 32-bit integer
	Integer16                  // signed 16-bit integer
	Integer8                   // signed 8-bit integer
	Float64                    // signed IEEE 754 64-bit floating-point number
	Float32                    // signed IEEE 754 32-bit floating-point number
	Unsigned64                 // unsigned 64-bit integer
	Unsigned32                 // unsigned 32-bit integer
	Unsigned16                 // unsigned 16-bit integer
	Unsigned8                  // unsigned 8-bit integer
	Boolean                    // unsigned 8-bit boolean (0 or 1, false or true)
	Character                  // Unicode code point (signed 32-bit integer, U+0000 ... U+10FFFF)
	String                     // Encoded string (sequence of UTF encoded characters)
)

// Data type bit flags for pointer and reference modifiers (bits 8+).
const (
	Pointer   DataType = 1 << 8 // bit 8: pointer type (ptr T)
	Reference DataType = 1 << 9 // bit 9: reference type (ref T)
)

type DataType int

// Return the plain data type without modifiers.
func (dt DataType) AsPlain() DataType {
	return dt & 0xFF
}

// Check whether the data type is a plain data type without modifiers.
func (dt DataType) IsPlain() bool {
	return dt == dt.AsPlain()
}

// Return the plain data type with a pointer modifier.
func (dt DataType) AsPointer() DataType {
	return dt.AsPlain() | Pointer
}

// Check whether the data type is a pointer type.
func (dt DataType) IsPointer() bool {
	return dt&Pointer != 0
}

// Return the plain data type with a reference modifier.
func (dt DataType) AsReference() DataType {
	return dt.AsPlain() | Reference
}

// Check whether the data type is a reference type.
func (dt DataType) IsReference() bool {
	return dt&Reference != 0
}

// Check whether the data type is untyped.
func (dt DataType) IsUntyped() bool {
	return dt.AsPlain() == Untyped
}

// Supported data types for symbol entries, temporaries, literals, and variables.
func (dt DataType) IsSupported() bool {
	return dt.AsPlain() >= Integer64 && dt.AsPlain() <= String
}

// Check whether the data type has a signed representation.
func (dt DataType) IsSigned() bool {
	return dt.AsPlain() >= Integer64 && dt.AsPlain() <= Float32
}

// Check whether the data type has an unsigned representation.
func (dt DataType) IsUnsigned() bool {
	return dt.AsPlain() >= Unsigned64 && dt.AsPlain() <= Boolean
}

// Check whether the data type is a signed integer.
func (dt DataType) IsSignedInteger() bool {
	return dt.AsPlain() >= Integer64 && dt.AsPlain() <= Integer8
}

// Check whether the data type is an unsigned integer.
func (dt DataType) IsUnsignedInteger() bool {
	return dt.AsPlain() >= Unsigned64 && dt.AsPlain() <= Unsigned8
}

// Check whether the data type is an integer.
func (dt DataType) IsInteger() bool {
	return dt.IsSignedInteger() || dt.IsUnsignedInteger()
}

// Check whether the data type is a floating point number.
func (dt DataType) IsFloatingPoint() bool {
	return dt.AsPlain() == Float64 || dt == Float32
}

// Check whether the data type is a boolean.
func (dt DataType) IsBoolean() bool {
	return dt.AsPlain() == Boolean
}

// Check whether the data type is a character.
func (dt DataType) IsCharacter() bool {
	return dt.AsPlain() == Character
}

// Check whether the data type is a string.
func (dt DataType) IsString() bool {
	return dt.AsPlain() == String
}

// String representation of a datatype with its modifiers.
func (dt DataType) String() string {
	var prefix string

	if dt.IsPointer() {
		prefix = pointerPrefix
	} else if dt.IsReference() {
		prefix = referencePrefix
	}

	return fmt.Sprintf("%v%v", prefix, dataTypeNames[dt.AsPlain()])
}

var (
	// Map an address datatype to its string representation that is based on the C language.
	dataTypeNames = map[DataType]string{
		Untyped:    "void",
		Integer64:  "int64",
		Integer32:  "int32",
		Integer16:  "int16",
		Integer8:   "int8",
		Float64:    "float64",
		Float32:    "float32",
		Unsigned64: "uint64",
		Unsigned32: "uint32",
		Unsigned16: "uint16",
		Unsigned8:  "uint8",
		Boolean:    "uint8",
		Character:  "codepoint",
		String:     "text",
	}
)

// Prefixes for pointer and reference modifier string representations.
const (
	pointerPrefix   = "ptr."
	referencePrefix = "ref."
)

