//====================================================================================
//                       Markov.h
//                            by Shinsuke MORI
//                            Last change : 9 June 1995
//====================================================================================

// 機  能 : マルコフモデル
//
// 注意点 : なし


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _Markov_h
#define _Markov_h

#ifdef DEBUG
#define Markov_DEBUG
#endif
//#define Markov_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <map>
#include <math.h>
#include <fcntl.h>
#include <errno.h>
#include <mystd.h>

typedef map<U_INT4, U_INT4, less<U_INT4> > STLMap;
typedef STLMap* STLMap_P;


//------------------------------------------------------------------------------------
//                       class Markov
//------------------------------------------------------------------------------------
class Markov{

  public:

             Markov();

             Markov(const string&);

//             Markov(const Markov&);

             ~Markov();

    void     read(const string&);

    S_INT4   cost(U_INT4, U_INT4) const;

    S_INT4   _1cost(U_INT4) const;

#ifdef Markov_DEBUG
    void     test() const;
#endif

  protected:

  private:

    U_INT4   size;

    S_INT4_P cost_1gram;                          // 補間後の 1-gram 確率

    STLMap_P cost_2gram;                          // 補間後の 2-gram 確率

};


//------------------------------------------------------------------------------------
//                       constractor
//------------------------------------------------------------------------------------

Markov::Markov()
: cost_1gram(NULL), cost_2gram(NULL)
{
#ifdef Markov_DEBUG
    cerr << "Markov::Markov()" << endl;
#endif

}

Markov::Markov(const string& filename)
{
#ifdef Markov_DEBUG
    cerr << "Markov::Markov(const string&)" << endl;
#endif

    read(filename);
}

/*
Markov::Markov(const Markov& markov)
{
    assert(0);

    cost_1gram = new U_INT4[size];                // 補間後の 1-gram 確率
    for (U_INT4 x = 0; x < size; x++){
        cost_1gram[x] = markov.cost_1gram[x];
    }

    cost_2gram = new STLMap[size];                // 補間後の 2-gram 確率
    for (U_INT4 y = 0; y < size; y++){
        cost_2gram[y] = markov.cost_2gram[y];
    }
}
*/


//------------------------------------------------------------------------------------
//                       destructor
//------------------------------------------------------------------------------------

Markov::~Markov()
{
#ifdef Markov_DEBUG
    cerr << "Markov::~Markov()" << endl;
#endif

//    if (cost_1gram != NULL) delete[] cost_1gram;
//    if (cost_2gram != NULL) delete[] cost_2gram;
}


//------------------------------------------------------------------------------------
//                       read
//------------------------------------------------------------------------------------

// 機  能 : ファイルからパラメータを読み込む

void Markov::read(const string& filename)
{
#ifdef Markov_DEBUG
    cerr << "Markov::read(const string&)" << endl;
#endif

//    if (cost_1gram != NULL) delete[] cost_1gram;
//    if (cost_2gram != NULL) delete[] cost_2gram;

    ifstream file(filename.c_str());
    if (! file) openfailed(filename);

    file.read(S_CHAR_P(&size), sizeof(S_INT4));   // 状態の数

    cost_1gram = new S_INT4[size];                // 補間後の 1-gram 確率
    file.read(S_CHAR_P(cost_1gram), size*sizeof(S_INT4));

    cost_2gram = new STLMap[size];                // 補間後の 2-gram 確率
    for (U_INT4 y = 0; y < size; y++){
        U_INT4 nonz;
        file.read(S_CHAR_P(&nonz), sizeof(U_INT4)); // 非ゼロ要素数
        for (U_INT4 i = 0; i < nonz; i++){
            U_INT4 suff;                          // 予測される形態素番号
            S_INT4 cost;                          // 対数確率値の定数倍
            file.read(S_CHAR_P(&suff), sizeof(U_INT4));
            file.read(S_CHAR_P(&cost), sizeof(S_INT4));
            cost_2gram[y][suff] = cost;
        }
    }

    file.close();
}

    
//------------------------------------------------------------------------------------
//                       cost
//------------------------------------------------------------------------------------

// 機  能 : 補間した遷移確率の負対数値を返す。
//          -log P(x|y)

inline S_INT4 Markov::cost(U_INT4 y, U_INT4 x) const
{
#ifdef Markov_DEBUG
    cerr << "Markov::cost(U_INT4, U_INT4)" << endl;
#endif

    STLMap::iterator iter = cost_2gram[y].find(x);
    if (iter != cost_2gram[y].end()){             // f(y, x) > 0 の場合
        return((*iter).second);
    }
    return(cost_1gram[x]);
}


//------------------------------------------------------------------------------------
//                       _1cost
//------------------------------------------------------------------------------------

// 機  能 : 単語1-gramモデルによるコストを返す。
//          -log P(x)

inline S_INT4 Markov::_1cost(U_INT4 x) const
{
#ifdef Markov_DEBUG
    cerr << "Markov::_1cost(U_INT4)" << endl;
#endif

    return(cost_1gram[x]);
}


//------------------------------------------------------------------------------------
//                       test
//------------------------------------------------------------------------------------

#ifdef Markov_DEBUG
void Markov::test() const
{
    for (U_INT4 x = 0; x < size; x++){
        cerr << form("cost(%4d) = %8d", x, cost_1gram[x]) << endl;
    }
    for (U_INT4 y = 0; y < size; y++){
        for (STLMap::iterator iter = cost_2gram[y].begin();
             iter != cost_2gram[y].end(); iter++){
            cerr << form("cost(%4d|%4d) = %8d", (*iter).first, y, (*iter).second)
              << endl;
        }
        cerr << form("y = %4d, size = %4d", y, cost_2gram[y].size()) << endl;
    }
}
#endif


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
