//====================================================================================
//                       mystd.h
//                            by Shinsuke MORI
//                            Last change : 19 July 1999
//====================================================================================

//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef	__mystd_h
#define	__mystd_h


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <cstdio>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>
#include <sstream>
#include <math.h>
#ifdef	_MATH_H

#include <assert.h>


//------------------------------------------------------------------------------------
//                       namespace
//------------------------------------------------------------------------------------

using namespace std;


//------------------------------------------------------------------------------------
//                       general typedifine
//------------------------------------------------------------------------------------

typedef void*            VOID_P;                  // void pointer
typedef void**           VOID_P_P;                // and pointer of it
typedef void***          VOID_P_P_P;              // and pointer of it

typedef char             S_CHAR;                  // signed character
typedef char*            S_CHAR_P;                // and pointer of it
typedef char**           S_CHAR_P_P;              // and pointer of it
typedef char***          S_CHAR_P_P_P;            // and pointer of it
typedef const char             C_S_CHAR;          // signed character
typedef const char*            C_S_CHAR_P;        // and pointer of it
typedef const char**           C_S_CHAR_P_P;      // and pointer of it
typedef const char***          C_S_CHAR_P_P_P;    // and pointer of it

typedef unsigned char    U_CHAR;                  // unsigned character
typedef unsigned char*   U_CHAR_P;                // and pointer of it
typedef unsigned char**  U_CHAR_P_P;              // and pointer of it
typedef unsigned char*** U_CHAR_P_P_P;            // and pointer of it
typedef const unsigned char    C_U_CHAR;          // unsigned character
typedef const unsigned char*   C_U_CHAR_P;        // and pointer of it
typedef const unsigned char**  C_U_CHAR_P_P;      // and pointer of it
typedef const unsigned char*** C_U_CHAR_P_P_P;    // and pointer of it

typedef char             S_INT1;                  // signed 1 byte integer
typedef char*            S_INT1_P;                // and pointer of it
typedef char**           S_INT1_P_P;              // and pointer of it
typedef char***          S_INT1_P_P_P;            // and pointer of it
typedef const char             C_S_INT1;          // signed 1 byte integer
typedef const char*            C_S_INT1_P;        // and pointer of it
typedef const char**           C_S_INT1_P_P;      // and pointer of it
typedef const char***          C_S_INT1_P_P_P;    // and pointer of it

#define S_INT1_MAX       +0x7f			  // maximum value of it
#define S_INT1_MIN       -0x7f			  // minimum value of it

typedef unsigned char    U_INT1;                  // unsigned 1 byte integer
typedef unsigned char*   U_INT1_P;                // and pointer of it
typedef unsigned char**  U_INT1_P_P;              // and pointer of it
typedef unsigned char*** U_INT1_P_P_P;            // and pointer of it
typedef const unsigned char    C_U_INT1;          // unsigned 1 byte integer
typedef const unsigned char*   C_U_INT1_P;        // and pointer of it
typedef const unsigned char**  C_U_INT1_P_P;      // and pointer of it
typedef const unsigned char*** C_U_INT1_P_P_P;    // and pointer of it
#define U_INT1_MAX       0xff			  // maximum value of it
#define U_INT1_MIN       0x00			  // minimum value of it

typedef short            S_INT2;                  // signed 2 bytes integer
typedef short*           S_INT2_P;                // and pointer of it
typedef short**          S_INT2_P_P;              // and pointer of it
typedef short***         S_INT2_P_P_P;            // and pointer of it
typedef const short            C_S_INT2;          // signed 2 bytes integer
typedef const short*           C_S_INT2_P;        // and pointer of it
typedef const short**          C_S_INT2_P_P;      // and pointer of it
typedef const short***         C_S_INT2_P_P_P;    // and pointer of it
#define S_INT2_MAX       +0x7fff		  // maximum value of it
#define S_INT2_MIN       -0x7fff		  // minimum value of it

typedef unsigned short   U_INT2;                  // unsigned 2 bytes integer
typedef unsigned short*  U_INT2_P;                // and pointer of it
typedef unsigned short** U_INT2_P_P;              // and pointer of it
typedef unsigned short***U_INT2_P_P_P;            // and pointer of it
typedef const unsigned short   C_U_INT2;          // unsigned 2 bytes integer
typedef const unsigned short*  C_U_INT2_P;        // and pointer of it
typedef const unsigned short** C_U_INT2_P_P;      // and pointer of it
typedef const unsigned short***C_U_INT2_P_P_P;    // and pointer of it
#define U_INT2_MAX       0xffff	        	  // maximum value of it
#define U_INT2_MIN       0x0000 		  // minimum value of it

typedef int              S_INT4;                  // signed 4 bytes integer
typedef int*             S_INT4_P;                // and pointer of it
typedef int**            S_INT4_P_P;              // and pointer of it
typedef int***           S_INT4_P_P_P;            // and pointer of it
typedef const int              C_S_INT4;          // signed 4 bytes integer
typedef const int*             C_S_INT4_P;        // and pointer of it
typedef const int**            C_S_INT4_P_P;      // and pointer of it
typedef const int***           C_S_INT4_P_P_P;    // and pointer of it
#define S_INT4_MAX       +0x7fffffff		  // maximum value of it
#define S_INT4_MIN       -0x7fffffff		  // minimum value of it

typedef unsigned int     U_INT4;                  // unsigned 4 bytes integer
typedef unsigned int*    U_INT4_P;                // and pointer of it
typedef unsigned int**   U_INT4_P_P;              // and pointer of it
typedef unsigned int***  U_INT4_P_P_P;            // and pointer of it
typedef const unsigned int     C_U_INT4;          // unsigned 4 bytes integer
typedef const unsigned int*    C_U_INT4_P;        // and pointer of it
typedef const unsigned int**   C_U_INT4_P_P;      // and pointer of it
typedef const unsigned int***  C_U_INT4_P_P_P;    // and pointer of it
#define U_INT4_MAX       0xffffffff		  // maximum value of it
#define U_INT4_MIN       0x00000000		  // minimum value of it

typedef float            DECIM4;                  // signed 4 bytes decimal
typedef float*           DECIM4_P;                // and pointer of it
typedef float**          DECIM4_P_P;              // and pointer of it
typedef float***         DECIM4_P_P_P;            // and pointer of it
typedef const float            C_DECIM4;          // signed 4 bytes decimal
typedef const float*           C_DECIM4_P;        // and pointer of it
typedef const float**          C_DECIM4_P_P;      // and pointer of it
typedef const float***         C_DECIM4_P_P_P;    // and pointer of it

typedef double           DECIM8;                  // signed 8 bytes decimal
typedef double*          DECIM8_P;                // and pointer of it
typedef double**         DECIM8_P_P;              // and pointer of it
typedef double***        DECIM8_P_P_P;            // and pointer of it
typedef const double           C_DECIM8;          // signed 8 bytes decimal
typedef const double*          C_DECIM8_P;        // and pointer of it
typedef const double**         C_DECIM8_P_P;      // and pointer of it
typedef const double***        C_DECIM8_P_P_P;    // and pointer of it
#define DECIM8_MAX       +HUGE
//#define DECIM8_MAX       +DBL_MAX
#define DECIM8_MIN       -HUGE

typedef bool             BOOL;                    // for bool variable
typedef bool*            BOOL_P;                  // and pointer of it
typedef bool**           BOOL_P_P;                // and pointer of it
typedef bool***          BOOL_P_P_P;              // and pointer of it
const BOOL TRUE = true;
const BOOL FALSE = false;
const BOOL VRAI = true;
const BOOL FAUX = false;


//------------------------------------------------------------------------------------
//                       general define
//------------------------------------------------------------------------------------

/*
#define SEEK_SET         0
#define SEEK_CUR         1
#define SEEK_END         2
*/

#define EOS              '\0'			  // End of String
#define EOL              '\n'			  // End of Line
#define EOT              ' '			  // End of Token
#define CHARLEN          0x100

#define KILO             0x400
#define MEGA             0x100000
#define GIGA             0x40000000


//------------------------------------------------------------------------------------
//                       perror
//------------------------------------------------------------------------------------

void perror(const string& str)
{
    perror(str.c_str());
}


//------------------------------------------------------------------------------------
//                       atof
//------------------------------------------------------------------------------------

DECIM8 atof(const string& str)
{
    return(atof(str.c_str()));
}


//------------------------------------------------------------------------------------
//                       atoi
//------------------------------------------------------------------------------------

S_INT4 atoi(const string& str)
{
    return(atoi(str.c_str()));
}


//------------------------------------------------------------------------------------
//                       openfailed
//------------------------------------------------------------------------------------

void openfailed(const string& str)
{
    perror("Can't open " + str);
    exit(-1);
}


//------------------------------------------------------------------------------------
//                       memalloced
//------------------------------------------------------------------------------------

inline string memalloced(U_INT4 size)
{
    stringstream temp;
    temp << "Memory Allocated ";
    if (size > GIGA){
        temp << setw(3) << size/GIGA << "[GB]";
    }else if (size > MEGA){
        temp << setw(3) << size/MEGA << "[MB]";
    }else if (size > KILO){
        temp << setw(3) << size/KILO << "[KB]";
    }else{
        temp << setw(3) << size      << "[ B]";
    }
    return(temp.str());
}


//------------------------------------------------------------------------------------
//                       readpara
//------------------------------------------------------------------------------------

// 機  能 : EOL EOL をデリミタとして istream is から文字列を読み込む。
//          読み込んだ文字数を返す。
//
// 注意点 : 末尾の EOL EOL は EOL になる

U_INT4 readpara(istream& is, string& str){
    str = "";
    for (S_CHAR cur, pre = EOS; is.get(cur); pre = cur){
        if (pre == EOL && cur == EOL) return(str.length());
        str += cur;
    }
    return(str.length());
}


//------------------------------------------------------------------------------------
//                       freq
//------------------------------------------------------------------------------------

// 機  能 : string 中の c の頻度
//
// 注意点 : なし

template<typename _CharT, typename _Traits, typename _Alloc>
inline int freq(const basic_string<_CharT, _Traits, _Alloc>& str, const _CharT c){
    int found = 0;

    for (typename basic_string<_CharT, _Traits, _Alloc>::iterator iter = str.begin();
         iter != str.end(); iter++){
        if (*iter == c) found++;
    }

    return(found);
}


//------------------------------------------------------------------------------------
//                       freq
//------------------------------------------------------------------------------------

int freq(const string& str, const S_CHAR c){
    int found = 0;
    for (unsigned int i = 0; i < str.length(); i++)
        if (str[i] == c) found++;
    return(found);
};


//------------------------------------------------------------------------------------
//                       operator*
//------------------------------------------------------------------------------------

// 機  能 : i 個の str の連接を返す
//
// 注意点 : なし

string operator*(const string& str, U_INT4 i){
    string result = "";
    if (i == 0) return(result);
    while (i-- > 0){
        result += str;
    }
    return(result);
}


//------------------------------------------------------------------------------------
//                       operator>>
//------------------------------------------------------------------------------------

// 機  能 : 
//
// 注意点 : なし

ifstream& operator>>(ifstream& i, DECIM8& x)
{
    string temp;
    i >> temp;
    x = atof(temp);
    return(i);
}


//------------------------------------------------------------------------------------
//                       stringprintf
//------------------------------------------------------------------------------------

string stringprintf(const S_CHAR format[], U_INT2 x)
{
    S_CHAR buffer[MEGA];

    sprintf(buffer, format, x);

    return(string(buffer));
}

string stringprintf(const S_CHAR format[], U_INT4 x)
{
    S_CHAR buffer[MEGA];

    sprintf(buffer, format, x);

    return(string(buffer));
}

string stringprintf(const S_CHAR format[], S_INT4 x)
{
    S_CHAR buffer[MEGA];

    sprintf(buffer, format, x);

    return(string(buffer));
}

string stringprintf(const S_CHAR format[], DECIM8 x)
{
    S_CHAR buffer[MEGA];

    sprintf(buffer, format, x);

    return(string(buffer));
}


string stringprintf(const string& format, U_INT2 x)
{
    S_CHAR buffer[MEGA];

    sprintf(buffer, format.c_str(), x);

    return(string(buffer));
}

string stringprintf(const string& format, U_INT4 x)
{
    S_CHAR buffer[MEGA];

    sprintf(buffer, format.c_str(), x);

    return(string(buffer));
}

string stringprintf(const string& format, DECIM8 x)
{
    S_CHAR buffer[MEGA];

    sprintf(buffer, format.c_str(), x);

    return(string(buffer));
}


//------------------------------------------------------------------------------------
//                       pow
//------------------------------------------------------------------------------------

// 機  能 : pow(U_INT4, U_INT4), log(S_INT4, S_INT4)
//
// 注意点 : なし

inline U_INT4 pow(U_INT4 x, U_INT4 y){
    return(U_INT4(pow(DECIM8(x), DECIM8(y))));
}

inline U_INT4 pow(S_INT4 x, U_INT4 y){
    return(U_INT4(pow(DECIM8(x), DECIM8(y))));
}

inline U_INT4 pow(U_INT4 x, S_INT4 y){
    return(U_INT4(pow(DECIM8(x), DECIM8(y))));
}

inline U_INT4 pow(S_INT4 x, S_INT4 y){
    return(U_INT4(pow(DECIM8(x), DECIM8(y))));
}


//------------------------------------------------------------------------------------
//                       log
//------------------------------------------------------------------------------------

// 機  能 : log(U_INT4), log(S_INT4)
//
// 注意点 : なし

inline DECIM8 log(U_INT4 x){
    return(log(DECIM8(x)));
}

inline DECIM8 log(S_INT4 x){
    return(log(DECIM8(x)));
}


//------------------------------------------------------------------------------------
//                       log2
//------------------------------------------------------------------------------------

inline DECIM8 log2(DECIM8 x){
    return(log(x)/log(2.0));
}

inline DECIM8 log2(U_INT4 x){
    return(log(DECIM8(x))/log(2.0));
}

#endif // _MATH_H


//------------------------------------------------------------------------------------
//                       split
//------------------------------------------------------------------------------------

template <class charT, class traits, class Allocator>
int split(const basic_string <charT, traits, Allocator>& x,
          basic_string <charT, traits, Allocator> res[],
          int maxn, const charT sep)
{
    int n = 0;
    int beg = 0;
    int end = 0;
    while (n < maxn){
        end = x.find(sep, beg);
        if (end == x.npos){
            res[n] = x.substr(beg);
            n++;
            return(n);
        }
        res[n] = x.substr(beg, end-beg);
        beg = end+1;
        n++;
    }
    return(n);
}


//------------------------------------------------------------------------------------
//                       readline
//------------------------------------------------------------------------------------

template <class charT, class traits, class Allocator>
int readline(istream& s, basic_string <charT, traits, Allocator>& x,
             charT terminator = '\n')
{
    getline(s, x, terminator);
    return(x.length());
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif // !__mystd_h


//====================================================================================
//                       END
//====================================================================================
