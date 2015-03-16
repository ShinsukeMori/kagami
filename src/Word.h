//====================================================================================
//                       Word.h
//                            by Shinsuke MORI
//                            Last change : 23 November 2005
//====================================================================================

// 機  能 : 単語
//
// 注意点 : なし


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _Word_h
#define _Word_h 1

#ifdef DEBUG
#define Word_DEBUG
#endif
//#define Word_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <vector>
#include <mystd.h>
#include <W_CHAR.h>


//------------------------------------------------------------------------------------
//                       typedef
//------------------------------------------------------------------------------------

typedef vector<W_CHAR> WORD;


//------------------------------------------------------------------------------------
//                       prototype
//------------------------------------------------------------------------------------

ostream& operator<<(ostream&, const WORD&);
WORD join(const W_CHAR&, const vector<WORD>&);
WORD join(const WORD&, const vector<WORD>&);
WORD STR2WORD(const S_CHAR_P);


//------------------------------------------------------------------------------------
//                       constant
//------------------------------------------------------------------------------------

const WORD BTword(1, BTwc);
const WORD WSword(1, WSwc);

//WORD(const S_CHAR_P)


//------------------------------------------------------------------------------------
//                       join
//------------------------------------------------------------------------------------

// 機  能 : 単語の接続
//
// 注意点 : なし

WORD join(const W_CHAR& deli, const vector<WORD>& word)
{
    WORD result;

    vector<WORD>::const_iterator ix = word.begin();
    if (ix != word.end()){
        for (WORD::const_iterator iy = (*ix).begin(); iy != (*ix).end(); iy++){
            result.push_back(*iy);
        }
    }

    for (ix++; ix != word.end(); ix++){
        result.push_back(deli);
        for (WORD::const_iterator iy = (*ix).begin(); iy != (*ix).end(); iy++){
            result.push_back(*iy);
        }
    }

    return(result);
}

WORD join(const WORD& deli, const vector<WORD>& word)
{
    WORD result;

    vector<WORD>::const_iterator ix = word.begin();
    if (ix != word.end()){
        for (WORD::const_iterator iy = (*ix).begin(); iy != (*ix).end(); iy++){
            result.push_back(*iy);
        }
    }

    for (ix++; ix != word.end(); ix++){
        for (WORD::const_iterator iy = deli.begin(); iy != deli.end(); iy++){
            result.push_back(*iy);
        }
        for (WORD::const_iterator iy = (*ix).begin(); iy != (*ix).end(); iy++){
            result.push_back(*iy);
        }
    }

    return(result);
}


//------------------------------------------------------------------------------------
//                       operator<<
//------------------------------------------------------------------------------------

// 機  能 : 単語の表示
//
// 注意点 : なし

ostream& operator<<(ostream& os, const WORD& word)
{
    for (WORD::const_iterator iter = word.begin(); iter != word.end(); iter++){
        os << *iter;
    }

    return os;
}


//------------------------------------------------------------------------------------
//                       STR2WORD
//------------------------------------------------------------------------------------

// 機  能 : 単語の表示
//
// 注意点 : WORD をクラスにしてコンストラクターで対応か??

WORD STR2WORD(const string& str){
    assert(str.length()%2 == 0);

    WORD word(str.length()/2);

    for (U_INT4 suff = 0; suff < str.length()/2; suff++){
        word[suff] = W_CHAR(str[suff*2], str[suff*2+1]);
    }

    return(word);
}

WORD STR2WORD(const S_CHAR_P str){
    if (strlen(str)%2 != 0){
        cerr << "|str| = " << strlen(str) << endl;
        cerr << "str = " << str << endl;
    }
    assert(strlen(str)%2 == 0);

    WORD word(strlen(str)/2);

    for (U_INT4 suff = 0; suff < strlen(str)/2; suff++){
        word[suff] = W_CHAR(str+suff*2);
    }

    return(word);
}

WORD STR2WORD(W_String wstr, U_INT4 from, U_INT4 length){
    WORD word(length);

    for (U_INT4 suff = 0; suff < length; suff++){
        word[suff] = wstr[from+suff];
    }

    return(word);
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
