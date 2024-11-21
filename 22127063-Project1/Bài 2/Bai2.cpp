#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <string>
#include <iomanip>
#include <stdexcept>
#include <sstream>

using namespace std;

struct BigInt {
    bool sign;  // true for positive, false for negative
    vector<uint32_t> BigIntDigits;
};

const uint32_t BASE = 1000000000; // Base for storing digits (9 decimal digits)

// Convert hex character to integer
uint32_t hexToInt(char hexChar) {
    if (hexChar >= '0' && hexChar <= '9') {
        return hexChar - '0';
    } else if (hexChar >= 'A' && hexChar <= 'F') {
        return hexChar - 'A' + 10;
    } else if (hexChar >= 'a' && hexChar <= 'f') {
        return hexChar - 'a' + 10;
    } else {
        throw invalid_argument("Invalid hex character");
    }
}

// Multiply BigInt by 16
void multiplyBy16(BigInt& bigInt) {
    uint64_t carry = 0;
    for (auto& digit : bigInt.BigIntDigits) {
        uint64_t product = static_cast<uint64_t>(digit) * 16 + carry;
        digit = product % BASE;
        carry = product / BASE;
    }
    if (carry > 0) {
        bigInt.BigIntDigits.push_back(static_cast<uint32_t>(carry));
    }
}

// Add a single digit to BigInt
void addDigit(BigInt& bigInt, uint32_t digit) {
    uint64_t carry = digit;
    for (auto& element : bigInt.BigIntDigits) {
        uint64_t sum = static_cast<uint64_t>(element) + carry;
        element = sum % BASE;
        carry = sum / BASE;
        if (carry == 0) return;
    }
    if (carry > 0) {
        bigInt.BigIntDigits.push_back(static_cast<uint32_t>(carry));
    }
}

// Convert hex string to BigInt
BigInt hexToBigInt(const string& hex) {
    BigInt result = {true, {0}};
    for (char hexChar : hex) {
        multiplyBy16(result);
        addDigit(result, hexToInt(hexChar));
    }
    return result;
}

// Print BigInt
void printBigInt(const BigInt& bigInt) {
    if (bigInt.BigIntDigits.empty()) {
        cout << "0\n";
        return;
    }
    if (!bigInt.sign) cout << "-";

    // Print the most significant digit without leading zeros
    cout << bigInt.BigIntDigits.back();

    // Print the remaining digits with leading zeros
    for (int i = bigInt.BigIntDigits.size() - 2; i >= 0; --i) {
        cout << setw(9) << setfill('0') << bigInt.BigIntDigits[i];
    }
    cout << endl;
}


// Compare two BigInts: -1 if a < b, 0 if a == b, 1 if a > b
int compareBigInts(const BigInt& a, const BigInt& b) {
    // Xét dấu trước
    if (a.sign != b.sign) return a.sign ? 1 : -1;

    // Nếu cùng dấu, so sánh phần giá trị
    int magnitudeComparison = 0;
    if (a.BigIntDigits.size() > b.BigIntDigits.size()) magnitudeComparison = 1;
    else if (a.BigIntDigits.size() < b.BigIntDigits.size()) magnitudeComparison = -1;
    else {
        for (size_t i = a.BigIntDigits.size(); i-- > 0;) {
            if (a.BigIntDigits[i] > b.BigIntDigits[i]) {
                magnitudeComparison = 1;
                break;
            }
            if (a.BigIntDigits[i] < b.BigIntDigits[i]) {
                magnitudeComparison = -1;
                break;
            }
        }
    }

    // Nếu là số âm, đảo kết quả so sánh
    return a.sign ? magnitudeComparison : -magnitudeComparison;
}

BigInt subtractBigInts(const BigInt& a, const BigInt& b);

BigInt addBigInts(const BigInt& a, const BigInt& b) {
    if (a.sign == b.sign) {
        // Nếu cùng dấu, thực hiện cộng
        BigInt result = {a.sign, {}};
        uint64_t carry = 0;
        size_t maxSize = max(a.BigIntDigits.size(), b.BigIntDigits.size());
        result.BigIntDigits.resize(maxSize);

        for (size_t i = 0; i < maxSize; ++i) {
            uint64_t sum = carry;
            if (i < a.BigIntDigits.size()) sum += a.BigIntDigits[i];
            if (i < b.BigIntDigits.size()) sum += b.BigIntDigits[i];
            result.BigIntDigits[i] = sum % BASE;
            carry = sum / BASE;
        }
        if (carry > 0) {
            result.BigIntDigits.push_back(static_cast<uint32_t>(carry));
        }
        return result;
    } else {
        // Nếu khác dấu, chuyển về phép trừ
        if (!a.sign) {
            // a âm, b dương => b - |a|
            return subtractBigInts(b, {true, a.BigIntDigits});
        } else {
            // a dương, b âm => a - |b|
            return subtractBigInts(a, {true, b.BigIntDigits});
        }
    }
}


BigInt subtractBigInts(const BigInt& a, const BigInt& b) {
    if (!a.sign && !b.sign) {
        // Nếu cả hai là số âm, đảo phép trừ: (-a) - (-b) => b - a
        return subtractBigInts(b, a);
    }

    if (a.sign != b.sign) {
        // Nếu khác dấu: a - (-b) => a + b
        return addBigInts(a, {a.sign, b.BigIntDigits});
    }

    // Nếu cùng dấu, kiểm tra giá trị lớn hơn
    if (compareBigInts(a, b) < 0) {
        BigInt result = subtractBigInts(b, a);
        result.sign = !a.sign; // Kết quả đảo dấu
        return result;
    }

    // Thực hiện trừ thông thường
    BigInt result = a;
    int64_t borrow = 0;

    for (size_t i = 0; i < b.BigIntDigits.size() || borrow > 0; ++i) {
        int64_t sub = borrow + (i < b.BigIntDigits.size() ? b.BigIntDigits[i] : 0);
        if (result.BigIntDigits[i] < sub) {
            result.BigIntDigits[i] += BASE - sub;
            borrow = 1;
        } else {
            result.BigIntDigits[i] -= sub;
            borrow = 0;
        }
    }

    // Xóa các chữ số 0 dư thừa
    while (result.BigIntDigits.size() > 1 && result.BigIntDigits.back() == 0) {
        result.BigIntDigits.pop_back();
    }

    return result;
}

BigInt multiplyBigInts(const BigInt& a, const BigInt& b) {
    if (a.BigIntDigits.empty() || b.BigIntDigits.empty()) return {true, {0}};
    BigInt result = {true, vector<uint32_t>(a.BigIntDigits.size() + b.BigIntDigits.size(), 0)};

    // Handle sign
    result.sign = (a.sign == b.sign);

    for (size_t i = 0; i < a.BigIntDigits.size(); ++i) {
        uint64_t carry = 0;
        for (size_t j = 0; j < b.BigIntDigits.size(); ++j) {
            uint64_t product = static_cast<uint64_t>(a.BigIntDigits[i]) * b.BigIntDigits[j] + result.BigIntDigits[i + j] + carry;
            result.BigIntDigits[i + j] = product % BASE;
            carry = product / BASE;
        }
        if (carry > 0) {
            result.BigIntDigits[i + b.BigIntDigits.size()] += static_cast<uint32_t>(carry);
        }
    }

    // Remove leading zeros
    while (result.BigIntDigits.size() > 1 && result.BigIntDigits.back() == 0) {
        result.BigIntDigits.pop_back();
    }

    return result;
}

pair<BigInt, BigInt> divideBigInts(const BigInt& dividend, const BigInt& divisor) {
    if (divisor.BigIntDigits.empty() || (divisor.BigIntDigits.size() == 1 && divisor.BigIntDigits[0] == 0)) {
        throw invalid_argument("Division by zero");
    }

    if (compareBigInts(dividend, divisor) < 0) {
        return {{true, {0}}, dividend}; // Thương = 0, Dư = dividend
    }

    BigInt quotient = {dividend.sign == divisor.sign, vector<uint32_t>(dividend.BigIntDigits.size(), 0)};
    BigInt remainder = {dividend.sign, dividend.BigIntDigits};

    size_t shift = dividend.BigIntDigits.size() - divisor.BigIntDigits.size();
    BigInt shiftedDivisor = divisor;
    shiftedDivisor.BigIntDigits.insert(shiftedDivisor.BigIntDigits.begin(), shift, 0);

    for (size_t i = shift + 1; i-- > 0;) {
        uint32_t q = 0;
        uint32_t low = 0, high = BASE - 1;

        while (low <= high) {
            uint32_t mid = low + (high - low) / 2;
            BigInt testProduct = multiplyBigInts(shiftedDivisor, {true, {mid}});

            if (compareBigInts(testProduct, remainder) <= 0) {
                q = mid;
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }

        quotient.BigIntDigits[i] = q;
        remainder = subtractBigInts(remainder, multiplyBigInts(shiftedDivisor, {true, {q}}));

        if (i > 0) {
            shiftedDivisor.BigIntDigits.erase(shiftedDivisor.BigIntDigits.begin());
        }
    }

    // Xóa các chữ số 0 dư thừa
    while (quotient.BigIntDigits.size() > 1 && quotient.BigIntDigits.back() == 0) {
        quotient.BigIntDigits.pop_back();
    }
    while (remainder.BigIntDigits.size() > 1 && remainder.BigIntDigits.back() == 0) {
        remainder.BigIntDigits.pop_back();
    }

    return {quotient, remainder};
}


BigInt minBigInts(const BigInt& a, const BigInt& b) {
    return (compareBigInts(a, b) < 0) ? a : b;
}

// Hàm modBigInts
BigInt modBigInts(const BigInt& a, const BigInt& b) {
    if (b.BigIntDigits.empty() || (b.BigIntDigits.size() == 1 && b.BigIntDigits[0] == 0)) {
        throw invalid_argument("Modulo by zero");
    }

    BigInt remainder = divideBigInts(a, b).second;

    // Nếu phần dư âm, cộng thêm `b`
    if (!remainder.sign) {
        remainder = addBigInts(remainder, b);
    }

    return remainder;
}

// Extended Euclidean Algorithm to find gcd and coefficients for BigInt
tuple<BigInt, BigInt, BigInt> extendedGCD(const BigInt& a, const BigInt& b) {
    // Base case
    if (compareBigInts(b, {true, {0}}) == 0) {
        return make_tuple(a, BigInt{true, {1}}, BigInt{true, {0}});
    }

    tuple<BigInt, BigInt, BigInt> result = extendedGCD(b, modBigInts(a, b));  // Recursive call
    BigInt gcd = get<0>(result);
    BigInt x1 = get<1>(result);
    BigInt y1 = get<2>(result);

    // Update coefficients
    BigInt x = y1;
    BigInt quotient = divideBigInts(a, b).first;
    BigInt y = subtractBigInts(x1, multiplyBigInts(quotient, y1));  // Assume BigInt division and subtraction are implemented
    
    return make_tuple(gcd, x, y);
}

// Modular Inverse for BigInt
BigInt modInverse(const BigInt& e, const BigInt& phi) {
    tuple<BigInt, BigInt, BigInt> result = extendedGCD(e, phi);
    BigInt gcd = get<0>(result);
    BigInt x = get<1>(result);

    if (compareBigInts(gcd, {true, {1}}) != 0) {
        throw invalid_argument("e and φ(N) are not coprime, no modular inverse exists.");
    }

    BigInt modInverse = modBigInts(x, phi);
    modInverse = addBigInts(modInverse, phi);
    modInverse = modBigInts(modInverse, phi);

    // Ensure positive modular inverse
    return modInverse;
}

string BigIntToHex(const BigInt& bigInt) {
    if (bigInt.BigIntDigits.empty()) {
        return "0"; // Handle empty BigInt as zero
    }

    string hexString;

    // Traverse digits in reverse (to handle big-endian output)
    for (int i = bigInt.BigIntDigits.size() - 1; i >= 0; --i) {
        stringstream ss;
        ss << hex << uppercase << bigInt.BigIntDigits[i];

        string hexPart = ss.str();

        // Ensure each part is padded to 9 hex digits (except the most significant one)
        if (i != bigInt.BigIntDigits.size() - 1) {
            while (hexPart.length() < 9) {
                hexPart = "0" + hexPart;
            }
        }

        hexString += hexPart;
    }

    // Remove leading zeros in the final hexadecimal string
    size_t firstNonZero = hexString.find_first_not_of('0');
    if (firstNonZero != string::npos) {
        hexString = hexString.substr(firstNonZero);
    } else {
        hexString = "0"; // Handle case where the number is zero
    }

    return hexString;
}


// Đọc file test.inp và trả về các giá trị BigInt của p, q, và e
void readInputFile(const string& inputFile, BigInt& p, BigInt& q, BigInt& e) {
    ifstream fin(inputFile);
    if (!fin.is_open()) {
        throw runtime_error("Cannot open file " + inputFile);
    }

    string pHex, qHex, eHex;
    getline(fin, pHex);
    getline(fin, qHex);
    getline(fin, eHex);

    // Chuyển đổi từ thập lục phân big-endian sang BigInt
    p = hexToBigInt(pHex);
    q = hexToBigInt(qHex);
    e = hexToBigInt(eHex);

    fin.close();
}

// Ghi kết quả ra file test.out
void writeOutputFile(const string& outputFile, const BigInt& d, bool success) {
    ofstream fout(outputFile);
    if (!fout.is_open()) {
        throw runtime_error("Cannot open file " + outputFile);
    }

    if (success) {
        string dHex = BigIntToHex(d);
        fout << dHex << endl;
    }
    else fout << "-1" << endl;

    fout.close();
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
        return 1;
    }

    string inputFile = argv[1];
    string outputFile = argv[2];

    BigInt p, q, e;
    readInputFile(inputFile, p, q, e);

    if (compareBigInts(e, minBigInts(p, q)) >= 0) {
        cout << "e must be smaller than min(p, q)" << endl;
        return 1;
    }

    BigInt one = {true, {1}};
    BigInt pMinus1 = subtractBigInts(p, one);
    BigInt qMinus1 = subtractBigInts(q, one);
    BigInt phi = multiplyBigInts(pMinus1, qMinus1);

    // Tìm d bằng modular inverse
    BigInt d;
    bool success = true;
    try {
        d = modInverse(e, phi);
    } catch (const invalid_argument&) {
        success = false;
    }

    // Ghi kết quả ra file
    writeOutputFile(outputFile, d, success);

    return 0;
}

// int main(){
//     string pHex = "9D", qHex = "C1", eHex = "17";

//     BigInt p = hexToBigInt(pHex);
//     cout << "p: ";
//     printBigInt(p);
//     BigInt q = hexToBigInt(qHex);
//     cout << "q: ";
//     printBigInt(q);
//     BigInt e = hexToBigInt(eHex);
//     cout << "e: ";
//     printBigInt(e);

//     if (compareBigInts(e, minBigInts(p, q)) >= 0) {
//         cout << "e must be smaller than min(p, q)" << endl;
//         return 1;
//     }

//     BigInt one = {true, {1}};
//     // Tính n và phi(n)
//     BigInt pMinus1 = subtractBigInts(p, one);
//     cout << "(p - 1): ";
//     printBigInt(pMinus1);
//     BigInt qMinus1 = subtractBigInts(q, one);
//     cout << "(q - 1): ";
//     printBigInt(qMinus1);

//     // Tính phi(n) = (p - 1)(q - 1)
//     BigInt phi = multiplyBigInts(pMinus1, qMinus1);
//     cout << "(p - 1)(q - 1): ";
//     printBigInt(phi);

//     // Tính khoá bí mật d
//     try{
//         BigInt d = modInverse(e, phi);
//         cout << "d: ";
//         printBigInt(d);

//         string output = BigIntToHex(d);
//         cout << output;
//     } catch (const invalid_argument& ex){
//         cerr << ex.what();
//         return 1;
//     }

//     return 0;
// }