//====================================================================================
//                       Connection.h
//                            by Shinsuke MORI
//                            Last change : 24 April 1996
//====================================================================================

// 機  能 : 外部プロセスとの通信のためのバッファー
//
// 注意点 : cf. src-util/agent.c struct connection


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _Connection_h
#define _Connection_h

#ifdef DEBUG
#define Connection_DEBUG
#endif
//#define Connection_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <math.h>
#include <mystd.h>
#include <minmax.h>

/*
#include "UkWord.h"
#include "Markov.h"
#include "IntStr.h"
#include "InMorp.h"
#include "ExMorp.h"
*/


//------------------------------------------------------------------------------------
//                       class Connection
//------------------------------------------------------------------------------------

class Connection{

  public:

    string rbuf;                                  // 外部プロセスからの読み込み
    //    int n_rbuf;
    //    int s_rbuf;
    //    int rfd;

    string wbuf;                                  // 外部プロセスへの書き出し
    //    int n_wbuf;
    //    int s_wbuf;
    //    int wfd;

    S_INT4 argc;                                   // 要素数

           Connection();

    void   init();

    S_INT4 operator()(istream&);

    void   operator()(ostream&);


    string argv(S_INT4);

friend ostream& operator<<(ostream&, const Connection&);

friend istream& operator>>(istream&, Connection&);

  private:

};


//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

inline Connection::Connection()
: rbuf(""), wbuf(""), argc(0)
{
    ;                                             // No Operation
}


//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

inline void Connection::init()
{
    rbuf = "";
    wbuf = "";
}


//------------------------------------------------------------------------------------
//                       operator()
//------------------------------------------------------------------------------------

inline S_INT4 Connection::operator()(istream& is)
{
    return(readline(is, rbuf, EOL));
}


//------------------------------------------------------------------------------------
//                       operator()
//------------------------------------------------------------------------------------

inline void Connection::operator()(ostream& os)
{
    os << "Connection = (" << rbuf << ", " << wbuf << ")" << endl;
}


//------------------------------------------------------------------------------------
//                       argv
//------------------------------------------------------------------------------------

string Connection::argv(S_INT4 n)
{
    argc = freq(rbuf, EOT)+1;                        // 要素数

    string argv[argc];                               // 各要素を記憶するための一時変数
    split(rbuf, argv, argc, EOT);                    // 行をトークンに分解する

    return(argv[n]);
}


//------------------------------------------------------------------------------------
//                       operator<<
//------------------------------------------------------------------------------------

inline ostream& operator<<(ostream& s, const Connection& connection)
{
    s << "Connection = (" << connection.rbuf << ", " << connection.wbuf << ")";

    return(s);
}


//------------------------------------------------------------------------------------
//                       operator>>
//------------------------------------------------------------------------------------

inline istream& operator>>(istream& s, Connection& connection)
{
    s >> connection.rbuf;

    return(s);
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
