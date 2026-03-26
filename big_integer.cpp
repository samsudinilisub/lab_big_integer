// =============================================================
//  big_integer.cpp -- BigInteger class implementation
//
//  TASK: Implement all methods declared in big_integer.h
//  This stub file exists only so the project structure is clear.
//  Replace its contents with your implementation.
// =============================================================

#include "big_integer.h"

#include <algorithm>
#include <iomanip>
#include <stdexcept>

namespace {

constexpr int BASE = 1'000'000'000;

void trim_digits(std::vector<int>& a, bool& neg) {
    while (!a.empty() && a.back() == 0) {
        a.pop_back();
    }
    if (a.empty()) {
        neg = false;
    }
}

int cmp_abs(const std::vector<int>& a, const std::vector<int>& b) {
    if (a.size() != b.size()) {
        return a.size() < b.size() ? -1 : 1;
    }
    for (int i = static_cast<int>(a.size()) - 1; i >= 0; --i) {
        if (a[i] != b[i]) {
            return a[i] < b[i] ? -1 : 1;
        }
    }
    return 0;
}

std::vector<int> add_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> res;
    long long carry = 0;
    size_t n = std::max(a.size(), b.size());
    for (size_t i = 0; i < n || carry; ++i) {
        long long sum = carry;
        if (i < a.size()) sum += a[i];
        if (i < b.size()) sum += b[i];
        res.push_back(sum % BASE);
        carry = sum / BASE;
    }
    return res;
}

std::vector<int> sub_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> res;
    long long borrow = 0;
    for (size_t i = 0; i < a.size(); ++i) {
        long long diff = a[i] - borrow - (i < b.size() ? b[i] : 0);
        if (diff < 0) {
            diff += BASE;
            borrow = 1;
        } else {
            borrow = 0;
        }
        res.push_back(static_cast<int>(diff));
    }
    return res;
}

std::vector<int> mul_digit(const std::vector<int>& a, int d) {
    if (d == 0) return {};
    std::vector<int> res;
    long long carry = 0;
    for (size_t i = 0; i < a.size() || carry; ++i) {
        long long cur = carry + static_cast<long long>(i < a.size() ? a[i] : 0) * d;
        res.push_back(cur % BASE);
        carry = cur / BASE;
    }
    return res;
}

}

BigInteger::BigInteger() : negative_(false) {}

BigInteger::BigInteger(int value) : BigInteger(static_cast<long long>(value)) {}

BigInteger::BigInteger(long long value) {
    if (value < 0) {
        negative_ = true;
        unsigned long long uval = static_cast<unsigned long long>(-(value + 1)) + 1;
        while (uval > 0) {
            digits_.push_back(uval % BASE);
            uval /= BASE;
        }
    } else {
        negative_ = false;
        unsigned long long uval = value;
        while (uval > 0) {
            digits_.push_back(uval % BASE);
            uval /= BASE;
        }
    }
}

BigInteger::BigInteger(const std::string& str) : negative_(false) {
    if (str.empty()) return;

    size_t start = 0;
    if (str[0] == '-') {
        negative_ = true;
        start = 1;
    } else if (str[0] == '+') {
        start = 1;
    }

    for (int i = static_cast<int>(str.length()); i > static_cast<int>(start); i -= 9) {
        int len = std::min(9, i - static_cast<int>(start));
        int start_idx = i - len;
        std::string chunk = str.substr(start_idx, len);
        digits_.push_back(std::stoi(chunk));
    }

    trim_digits(digits_, negative_);
}

BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
    if (negative_ == rhs.negative_) {
        digits_ = add_abs(digits_, rhs.digits_);
    } else {
        if (cmp_abs(digits_, rhs.digits_) >= 0) {
            digits_ = sub_abs(digits_, rhs.digits_);
        } else {
            digits_ = sub_abs(rhs.digits_, digits_);
            negative_ = rhs.negative_;
        }
    }
    trim_digits(digits_, negative_);
    return *this;
}

BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
    if (negative_ != rhs.negative_) {
        digits_ = add_abs(digits_, rhs.digits_);
    } else {
        if (cmp_abs(digits_, rhs.digits_) >= 0) {
            digits_ = sub_abs(digits_, rhs.digits_);
        } else {
            digits_ = sub_abs(rhs.digits_, digits_);
            negative_ = !negative_;
        }
    }
    trim_digits(digits_, negative_);
    return *this;
}

BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
    if (digits_.empty() || rhs.digits_.empty()) {
        digits_.clear();
        negative_ = false;
        return *this;
    }

    std::vector<int> res(digits_.size() + rhs.digits_.size(), 0);
    for (size_t i = 0; i < digits_.size(); ++i) {
        long long carry = 0;
        for (size_t j = 0; j < rhs.digits_.size() || carry; ++j) {
            long long cur = res[i + j] +
                            static_cast<long long>(digits_[i]) *
                                (j < rhs.digits_.size() ? rhs.digits_[j] : 0) +
                            carry;
            res[i + j] = cur % BASE;
            carry = cur / BASE;
        }
    }

    digits_ = std::move(res);
    negative_ = (negative_ != rhs.negative_);
    trim_digits(digits_, negative_);
    return *this;
}

BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
    if (rhs.is_zero()) {
        throw std::runtime_error("Division by zero");
    }
    if (cmp_abs(digits_, rhs.digits_) < 0) {
        digits_.clear();
        negative_ = false;
        return *this;
    }

    std::vector<int> q;
    std::vector<int> current;
    bool dummy = false;

    for (int i = static_cast<int>(digits_.size()) - 1; i >= 0; --i) {
        current.insert(current.begin(), digits_[i]);
        trim_digits(current, dummy);

        int L = 0, R = BASE - 1;
        int ans = 0;
        while (L <= R) {
            int mid = L + (R - L) / 2;
            std::vector<int> temp = mul_digit(rhs.digits_, mid);
            if (cmp_abs(temp, current) <= 0) {
                ans = mid;
                L = mid + 1;
            } else {
                R = mid - 1;
            }
        }

        q.push_back(ans);
        std::vector<int> to_sub = mul_digit(rhs.digits_, ans);
        current = sub_abs(current, to_sub);
        trim_digits(current, dummy);
    }

    std::reverse(q.begin(), q.end());
    digits_ = q;
    negative_ = (negative_ != rhs.negative_);
    trim_digits(digits_, negative_);
    return *this;
}

BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
    BigInteger q = *this / rhs;
    *this -= (q * rhs);
    return *this;
}

BigInteger BigInteger::operator+(const BigInteger& rhs) const {
    BigInteger res = *this;
    res += rhs;
    return res;
}

BigInteger BigInteger::operator-(const BigInteger& rhs) const {
    BigInteger res = *this;
    res -= rhs;
    return res;
}

BigInteger BigInteger::operator*(const BigInteger& rhs) const {
    BigInteger res = *this;
    res *= rhs;
    return res;
}

BigInteger BigInteger::operator/(const BigInteger& rhs) const {
    BigInteger res = *this;
    res /= rhs;
    return res;
}

BigInteger BigInteger::operator%(const BigInteger& rhs) const {
    BigInteger res = *this;
    res %= rhs;
    return res;
}

BigInteger BigInteger::operator-() const {
    BigInteger res = *this;
    if (!res.digits_.empty()) {
        res.negative_ = !res.negative_;
    }
    return res;
}

BigInteger& BigInteger::operator++() {
    *this += BigInteger(1);
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger temp = *this;
    ++(*this);
    return temp;
}

BigInteger& BigInteger::operator--() {
    *this -= BigInteger(1);
    return *this;
}

BigInteger BigInteger::operator--(int) {
    BigInteger temp = *this;
    --(*this);
    return temp;
}

bool BigInteger::operator==(const BigInteger& rhs) const {
    if (digits_.empty() && rhs.digits_.empty()) return true;
    return negative_ == rhs.negative_ && digits_ == rhs.digits_;
}

bool BigInteger::operator!=(const BigInteger& rhs) const {
    return !(*this == rhs);
}

bool BigInteger::operator<(const BigInteger& rhs) const {
    if (digits_.empty() && rhs.digits_.empty()) return false;
    if (negative_ != rhs.negative_) return negative_;

    int cmp = cmp_abs(digits_, rhs.digits_);
    if (cmp == 0) return false;
    return negative_ ? (cmp > 0) : (cmp < 0);
}

bool BigInteger::operator>(const BigInteger& rhs) const {
    return rhs < *this;
}

bool BigInteger::operator<=(const BigInteger& rhs) const {
    return rhs >= *this;
}

bool BigInteger::operator>=(const BigInteger& rhs) const {
    return !(*this < rhs);
}

std::string BigInteger::to_string() const {
    if (digits_.empty()) return "0";

    std::string res;
    if (negative_) res += "-";

    res += std::to_string(digits_.back());
    for (int i = static_cast<int>(digits_.size()) - 2; i >= 0; --i) {
        std::string d = std::to_string(digits_[i]);
        res += std::string(9 - d.length(), '0') + d;
    }
    return res;
}

bool BigInteger::is_zero() const {
    return digits_.empty();
}

bool BigInteger::is_negative() const {
    return negative_;
}

BigInteger::operator bool() const {
    return !is_zero();
}

std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
    return os << value.to_string();
}

std::istream& operator>>(std::istream& is, BigInteger& value) {
    std::string s;
    if (is >> s) {
        value = BigInteger(s);
    }
    return is;
}
