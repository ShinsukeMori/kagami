//====================================================================================
//                       ExMorp.h
//                            by Shinsuke MORI
//                            Last change : 1 November 1995
//====================================================================================

// 機  能 : 外部辞書の形態素
//
// 注意点 : ほとんど構造体


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _ExMorp_h
#define _ExMorp_h


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <mystd.h>


//------------------------------------------------------------------------------------
//                       class ExMorp
//------------------------------------------------------------------------------------

class ExMorp;
typedef ExMorp* ExMorp_P;

class ExMorp{

  public:

    U_INT4 length;                                // 文字数

    U_INT4 text;		                  // 変換後の表記

    S_INT4 cost;                                  // 対数確率値

           ExMorp(U_INT4, U_INT4, S_INT4);        // コンストラクター

    void   fprint(ostream&);                      // インスタンスの表示

  private:

};


//------------------------------------------------------------------------------------
//                       ExMorp
//------------------------------------------------------------------------------------

ExMorp::ExMorp(U_INT4 length = 0, U_INT4 text = 0, S_INT4 cost = 0)
: length(length), text(text), cost(cost)
{
    ;                                             // No Operation
}


//------------------------------------------------------------------------------------
//                       fprint
//------------------------------------------------------------------------------------

void ExMorp::fprint(ostream& fout = cout)
{
    fout << "(" << length << ", " << text << ", " << cost << ")" << endl;
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
