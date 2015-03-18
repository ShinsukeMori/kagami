//====================================================================================
//                       UkWord.h
//                            by Shinsuke MORI
//                            Last change : 18 May 1996
//====================================================================================

// 機  能 : 文字 2-gram による未知語モデル
//
// 注意点 : 未知文字モデルのファイルステムが Part[BT+1 .. PART-1] に格納されていること
//          アクセスされる長さにかかわらず maxlen 文字のテーブルを確保


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


//------------------------------------------------------------------------------------
//                       class UkWord
//------------------------------------------------------------------------------------

class UkWord
{

  public:

                    UkWord(U_INT4);
    
           void     init();

           void     tran(W_CHAR);

#ifdef _StopChar_h
           U_INT4   lmax() const{return(vallen);};// StopChar を考慮した有効な長さ
#endif
    
    inline U_INT4   cost(U_INT4) const;

  private:

           U_INT4   curpos;                       // 現在の位置(文頭からの文字数)

           U_INT4   maxlen;                       // 長さの最大値

           U_INT4   wc2int[0x10000];              // 文字から状態への写像

           U_INT4   curr;                         // 今の状態

           Markov   markov;                       // 文字マルコフモデル

           U_INT4_P Cost;                         // 各位置での対数確率

           U_INT4   UTcost;                       // -log(P(Σ|UT))*MULT

#ifdef _StopChar_h
           W_CHAR   prev;                         // 直前の文字

           U_INT4   vallen;                       // StopChar を考慮した有効な長さ

           StopChar stopchar;                     // 入力文を分割する文字
#endif

};

    
//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

UkWord::UkWord(U_INT4 maxlen)
: curpos(0), maxlen(maxlen)
#ifdef _StopChar_h
, prev(U_INT2(0)), vallen(0), stopchar("StopChar")
#endif
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::UkWord(U_INT4)" << endl;
#endif

    curr = BT;

    markov.read("Char.data");

    Cost = new U_INT4[maxlen];

    for (U_INT4 full = 0; full < 0x10000; full++) wc2int[full] = 0;

    U_INT4 lineno;
    ifstream file("CharIntStr.text");
    if (! file) openfailed("CharIntStr.text");
    S_CHAR buff[4];                           // ファイル読み込みのためのバッファ
    for (lineno = 0; file.read(buff, 3); lineno++){
        assert(buff[2] == '\n');              // 一つの全角文字と改行コードのはず
        W_CHAR wc(buff);
        wc2int[wc.full] = lineno;
    }
    file.close();

    UTcost = U_INT4(log(6878-(lineno-2))*MULT); // 全角文字の一様分布
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

inline U_INT4 UkWord::cost(U_INT4 length) const
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::cost(U_INT4, U_INT4)" << endl;
#endif

    return(Cost[curpos-length]+markov.cost(curr, BT));
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
