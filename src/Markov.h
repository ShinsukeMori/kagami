//====================================================================================
//                       Markov.h
//                            by Shinsuke MORI
//                            Last change : 9 June 1995
//====================================================================================

// ��  ǽ : �ޥ륳�ե�ǥ�
//
// ����� : �ʤ�


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

    S_INT4_P cost_1gram;                          // ��ָ�� 1-gram ��Ψ

    STLMap_P cost_2gram;                          // ��ָ�� 2-gram ��Ψ

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

    cost_1gram = new U_INT4[size];                // ��ָ�� 1-gram ��Ψ
    for (U_INT4 x = 0; x < size; x++){
        cost_1gram[x] = markov.cost_1gram[x];
    }

    cost_2gram = new STLMap[size];                // ��ָ�� 2-gram ��Ψ
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

// ��  ǽ : �ե����뤫��ѥ�᡼�����ɤ߹���

void Markov::read(const string& filename)
{
#ifdef Markov_DEBUG
    cerr << "Markov::read(const string&)" << endl;
#endif

//    if (cost_1gram != NULL) delete[] cost_1gram;
//    if (cost_2gram != NULL) delete[] cost_2gram;

    ifstream file(filename.c_str());
    if (! file) openfailed(filename);

    file.read(S_CHAR_P(&size), sizeof(S_INT4));   // ���֤ο�

    cost_1gram = new S_INT4[size];                // ��ָ�� 1-gram ��Ψ
    file.read(S_CHAR_P(cost_1gram), size*sizeof(S_INT4));

    cost_2gram = new STLMap[size];                // ��ָ�� 2-gram ��Ψ
    for (U_INT4 y = 0; y < size; y++){
        U_INT4 nonz;
        file.read(S_CHAR_P(&nonz), sizeof(U_INT4)); // �󥼥����ǿ�
        for (U_INT4 i = 0; i < nonz; i++){
            U_INT4 suff;                          // ͽ¬�����������ֹ�
            S_INT4 cost;                          // �п���Ψ�ͤ������
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

// ��  ǽ : ��֤������ܳ�Ψ�����п��ͤ��֤���
//          -log P(x|y)

inline S_INT4 Markov::cost(U_INT4 y, U_INT4 x) const
{
#ifdef Markov_DEBUG
    cerr << "Markov::cost(U_INT4, U_INT4)" << endl;
#endif

    STLMap::iterator iter = cost_2gram[y].find(x);
    if (iter != cost_2gram[y].end()){             // f(y, x) > 0 �ξ��
        return((*iter).second);
    }
    return(cost_1gram[x]);
}


//------------------------------------------------------------------------------------
//                       _1cost
//------------------------------------------------------------------------------------

// ��  ǽ : ñ��1-gram��ǥ�ˤ�륳���Ȥ��֤���
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
