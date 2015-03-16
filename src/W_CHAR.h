//====================================================================================
//                       W_CHAR.h
//                            by Shinsuke MORI
//                            Last change : 8 June 1995
//====================================================================================

#ifndef _W_CHAR_h
#define _W_CHAR_h

#include "mystd.h"


//------------------------------------------------------------------------------------
//                       class
//------------------------------------------------------------------------------------

class W_String;

class W_CHAR{

  private:

  public:

    union{
        U_INT2                         full;
        struct{U_CHAR hi; U_CHAR lo; } half;
    };

// constructors

    inline                 W_CHAR();
    inline                 W_CHAR(const U_INT2);
    inline                 W_CHAR(const S_CHAR_P);
    inline                 W_CHAR(const S_CHAR, const S_CHAR);

// operators

    inline friend ostream& operator << (ostream& s, const W_CHAR& w);
    inline friend istream& operator >> (istream& s, W_CHAR& w);
    inline friend BOOL     operator == (const W_CHAR&, const W_CHAR&);
    inline friend BOOL     operator <= (const W_CHAR&, const W_CHAR&);
    inline friend BOOL     operator >= (const W_CHAR&, const W_CHAR&);
    inline friend BOOL     operator < (const W_CHAR&, const W_CHAR&);
    inline friend BOOL     operator > (const W_CHAR&, const W_CHAR&);
    inline        W_CHAR   operator = (const W_CHAR&);
    inline        W_CHAR   operator = (const U_INT2&);

// others

//                  void error(const S_CHAR_P, const S_CHAR_P) const;

};

typedef W_CHAR* W_CHAR_P;


//------------------------------------------------------------------------------------
//                       constant
//------------------------------------------------------------------------------------

const W_CHAR BTwc = "BT";
const W_CHAR WSwc = "  ";


//------------------------------------------------------------------------------------
//                       constractor
//------------------------------------------------------------------------------------

inline W_CHAR::W_CHAR()
{
    // nothing to be done
}

inline W_CHAR::W_CHAR(const S_CHAR_P s)
{
    half.hi = (U_CHAR)s[0];
    half.lo = (U_CHAR)s[1];
}

inline W_CHAR::W_CHAR(const U_INT2 w)
{
    full = w;
}

inline W_CHAR::W_CHAR(const S_CHAR hi, const S_CHAR lo)
{
    half.hi = hi;
    half.lo = lo;
}


//------------------------------------------------------------------------------------
//                       operator <<
//------------------------------------------------------------------------------------

inline ostream& operator << (ostream& s, const W_CHAR& w)
{
    s << w.half.hi << w.half.lo;
    return(s);
}


//------------------------------------------------------------------------------------
//                       operator <<
//------------------------------------------------------------------------------------

inline istream& operator<<(istream& s, W_CHAR& w)
{
    s >> w.half.hi >> w.half.lo;

    return(s);
}


//------------------------------------------------------------------------------------
//                       Èæ³Ó±é»»»Ò (==, <=, >=, <, >)
//------------------------------------------------------------------------------------

inline BOOL operator == (const W_CHAR& wc1, const W_CHAR& wc2)
{
    if (wc1.full == wc2.full){
        return(TRUE);
    }else{
        return(FALSE);
    }
}

inline BOOL operator != (const W_CHAR& wc1, const W_CHAR& wc2)
{
    if (wc1.full == wc2.full){
        return(FALSE);
    }else{
        return(TRUE);
    }
}

/*
inline BOOL operator == (W_CHAR& wc1, const W_CHAR& wc2)
{
    if (wc1.full == wc2.full){
        return(TRUE);
    }else{
        return(FALSE);
    }
}

inline BOOL operator == (const W_CHAR& wc1, W_CHAR& wc2)
{
    if (wc1.full == wc2.full){
        return(TRUE);
    }else{
        return(FALSE);
    }
}

inline BOOL operator == (W_CHAR& wc1, W_CHAR& wc2)
{
    if (wc1.full == wc2.full){
        return(TRUE);
    }else{
        return(FALSE);
    }
}
*/

inline BOOL operator <= (const W_CHAR& wc1, const W_CHAR& wc2)
{
    if (wc1.half.hi < wc2.half.hi){
        return(TRUE);
    }
    if ((wc1.half.hi == wc2.half.hi) && (wc1.half.lo <= wc2.half.lo)){
        return(TRUE);
    }
    return(FALSE);
}

inline BOOL operator >= (const W_CHAR& wc1, const W_CHAR& wc2)
{
    if (wc1.half.hi > wc2.half.hi){
        return(TRUE);
    }
    if ((wc1.half.hi == wc2.half.hi) && (wc1.half.lo >= wc2.half.lo)){
        return(TRUE);
    }
    return(FALSE);
}


inline BOOL operator < (const W_CHAR& wc1, const W_CHAR& wc2)
{
    if (wc1.half.hi < wc2.half.hi){
        return(TRUE);
    }
    if ((wc1.half.hi == wc2.half.hi) && (wc1.half.lo < wc2.half.lo)){
        return(TRUE);
    }
    return(FALSE);
}

inline BOOL operator > (const W_CHAR& wc1, const W_CHAR& wc2)
{
    if (wc1.half.hi > wc2.half.hi){
        return(TRUE);
    }
    if ((wc1.half.hi == wc2.half.hi) && (wc1.half.lo > wc2.half.lo)){
        return(TRUE);
    }
    return(FALSE);
}


//------------------------------------------------------------------------------------
//                       operator =
//------------------------------------------------------------------------------------

inline W_CHAR W_CHAR::operator = (const W_CHAR& w)
{ 
    full = w.full;
    return(*this);
} 

inline W_CHAR W_CHAR::operator = (const U_INT2& w)
{ 
    full = w;
    return(*this);
} 


//------------------------------------------------------------------------------------
//                       error
//------------------------------------------------------------------------------------

//void W_CHAR::error(const S_CHAR_P string1, const S_CHAR_P string2)
//{
//    cerr << string1 << " " << string2 << "\n";
//    exit(1);
//}
    

//------------------------------------------------------------------------------------
//                       W_String
//------------------------------------------------------------------------------------

class W_String{

  public:

    W_CHAR_P wstr;

             W_String(const U_INT4&);

             W_String(const string&);

             const U_INT4 length();

    operator const S_CHAR_P() const;

    operator const W_CHAR_P() const;

    inline W_CHAR& operator[](const U_INT4);

    inline friend ostream& operator << (ostream&, const W_CHAR_P&);
    
    inline friend BOOL     operator == (W_String&, W_String&);


};

typedef W_String* W_String_P;


//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

inline W_String::W_String(const U_INT4& size)
{
    wstr = new W_CHAR[size];
}

inline W_String::W_String(const string& temp)
{
//    cerr << "W_String::W_String(const string& " << temp << ")" << endl;
//    cerr << "length = " << temp.length() << endl;

    wstr = new W_CHAR[temp.length()/2+1];
    strcpy(S_CHAR_P(wstr), temp.c_str());
}


//------------------------------------------------------------------------------------
//                       length
//------------------------------------------------------------------------------------

const U_INT4 W_String::length()
{
    U_INT4 length = 0;
    while (wstr[length].half.hi) length++;
    return(length);
}


//------------------------------------------------------------------------------------
//                       operator const S_CHAR_P()
//------------------------------------------------------------------------------------

inline W_String::operator const S_CHAR_P() const
{
    return(S_CHAR_P(wstr));
}


//------------------------------------------------------------------------------------
//                       operator const W_CHAR_P()
//------------------------------------------------------------------------------------

inline W_String::operator const W_CHAR_P() const
{
    return(W_CHAR_P(wstr));
}


//------------------------------------------------------------------------------------
//                       operator[]
//------------------------------------------------------------------------------------

inline W_CHAR& W_String::operator[](const U_INT4 suffix)
{
    return(wstr[suffix]);
}
    

//------------------------------------------------------------------------------------
//                       operator<<
//------------------------------------------------------------------------------------

inline ostream& operator<<(ostream& s, const W_String& w)
{
    for (W_CHAR_P wptr = w.wstr; (*wptr).half.hi; wptr++){
        s << (*wptr);
    }
    return(s);
}


//------------------------------------------------------------------------------------
//                       operator==
//------------------------------------------------------------------------------------

inline BOOL operator==(W_String& ws1, W_String& ws2)
{
    if (ws1.length() != ws2.length()){
        return(FAUX);
    }

    for (U_INT4 suff = 0; suff < ws1.length(); suff++){
        if (ws1[suff] != ws1[suff]){
            return(FAUX);
        }
    }

    return(VRAI);
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
