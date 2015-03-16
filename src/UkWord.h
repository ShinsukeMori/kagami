//====================================================================================
//                       UkWord.h
//                            by Shinsuke MORI
//                            Last change : 18 May 1996
//====================================================================================

// 機  能 : 文字 2-gram による未知語モデル
//
// 注意点 : アクセスされる長さにかかわらず maxlen 文字のテーブルを確保


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _UkWord_h
#define _UkWord_h

#ifdef DEBUG
#define UkWord_DEBUG
#endif
//#define UkWord_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <mystd.h>
#include "Markov.h"
#include "StopChar.h"
#include <Word.h>


//------------------------------------------------------------------------------------
//                       class UkWord
//------------------------------------------------------------------------------------

class UkWord
{

  public:

                    UkWord(U_INT4);

                    UkWord(U_INT4, string);
    
           void     init();

           void     tran(W_CHAR);

#ifdef _StopChar_h
           U_INT4   lmax() const{return(vallen);};// StopChar を考慮した有効な長さ
#endif
    
    inline S_INT4   cost(U_INT4) const;

    inline S_INT4   cost(WORD&) const;

  private:

           U_INT4   curpos;                       // 現在の位置(文頭からの文字数)

           U_INT4   maxlen;                       // 長さの最大値

           U_INT4   wc2int[0x10000];              // 各品詞の文字から状態への写像

           U_INT4   curr;                         // 各品詞の今の状態

           Markov   markov;                       // 各品詞の文字マルコフモデル

           S_INT4_P Cost;                         // 各品詞の各位置での対数確率

           S_INT4   UTcost;                       // 各品詞の -log(P(Σ|UT))

#ifdef _StopChar_h
           W_CHAR   prev;                         // 直前の文字

           U_INT4   vallen;                       // StopChar を考慮した有効な長さ

           StopChar stopchar;                     // 入力文を分割する文字
#endif

};

    
//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

UkWord::UkWord(U_INT4 maxlen, string path = "")
: curpos(0), maxlen(maxlen)
#ifdef _StopChar_h
, prev(U_INT2(0)), vallen(0), stopchar(path + "StopChar")
#endif
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::UkWord(U_INT4)" << endl;
#endif

    curr = BT;
        
    markov.read(path + "Char.data");

    Cost = new S_INT4[maxlen];

    for (U_INT4 full = 0; full < 0x10000; full++) wc2int[full] = 0;

    U_INT4 lineno;

    ifstream file((path + "CharIntStr.text").c_str());
    if (! file) openfailed(path + "CharIntStr.text");
    S_CHAR buff[4];                               // ファイル読み込みのためのバッファ
    for (lineno = 0; file.read(buff, 3); lineno++){
        assert(buff[2] == '\n');                  // 一つの全角文字と改行コードのはず
        W_CHAR wc(buff);
        wc2int[wc.full] = lineno;
    }
    file.close();

    UTcost = S_INT4(log(DECIM8(6878)-(lineno-2))*MULT); // 全角文字の一様分布
}

    
//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

inline void UkWord::init()
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::init()" << endl;
#endif

    curr = BT;

    curpos = 0;
#ifdef _StopChar_h
    vallen = 0;
#endif
}


//------------------------------------------------------------------------------------
//                       tran
//------------------------------------------------------------------------------------

inline void UkWord::tran(const W_CHAR code)
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::tran(const W_CHAR)" << endl;
#endif

#ifdef _StopChar_h
    if (stopchar[code] || stopchar[prev]){        // StopChar かその直後の場合
        vallen = 1;                               // 有効な長さを１に設定
    }else{                                        // その他の場合
        vallen = min(vallen+1, UkWordMaxLen);     // UkWordMaxLen までの範囲で inc
    }
#endif

    U_INT4 next = wc2int[code.full];              // 次の状態
#ifdef _StopChar_h
    for (U_INT4 i = max(vallen, curpos)-vallen; i < curpos; i++){
#else
    for (U_INT4 i = max(UkWordMaxLen, curpos)-UkWordMaxLen; i < curpos; i++){
#endif
        Cost[i] += markov.cost(curr, next);
        if (next == UT){                          // 未知文字の場合
            Cost[i] += UTcost;
        }
    }
    Cost[curpos] = markov.cost(BT, next);
    if (next == UT){                              // 未知文字の場合
        Cost[curpos] += UTcost;
    }
    curr = next;

#ifdef _StopChar_h
    prev = code;
#endif

    curpos++;
}


//------------------------------------------------------------------------------------
//                       cost
//------------------------------------------------------------------------------------

inline S_INT4 UkWord::cost(U_INT4 length) const
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::cost(U_INT4)" << endl;
#endif

    return(Cost[curpos-length]+markov.cost(curr, BT));
}


//------------------------------------------------------------------------------------

inline S_INT4 UkWord::cost(WORD& word) const
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::cost(WORD&)" << endl;
#endif

    S_INT4 cost = 0;
    U_INT4 curr = BT;

    for (U_INT4 suff = 0; suff < word.size(); suff++){
        U_INT4 next;
        next = wc2int[word[suff].full];           // 次の状態
        cost += markov.cost(curr, next);
        if (next == UT){                          // 未知文字の場合
            cost += UTcost;
        }
        curr = next;
    }
    cost += markov.cost(curr, BT);

    return(cost);
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
