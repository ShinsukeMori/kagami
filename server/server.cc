//====================================================================================
//                       main.cc
//                            by Shinsuke MORI
//                            Last change : 4 March 1995
//====================================================================================

// 機  能 : 形態素 2-gram モデルによる仮名漢字変換器
//
// 使用法 : main < (filename)
//
// 実  例 : main < ../../corpus/EDR10.senten
//
// 注意点 : なし


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#define EXDICT                                    // 外部辞書も使用

#define AC
#define MEMORY

#ifdef DEBUG
#define MAIN_DEBUG
#endif
//#define MAIN_DEBUG                                // 辞書引きの結果の表示


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <mystd.h>
#include <W_CHAR.h>

#include "constant.h"
#include "KKConv.h"
#include "Connection.h"


//------------------------------------------------------------------------------------
//                       prototypes
//------------------------------------------------------------------------------------

void usage(S_CHAR_P);


//------------------------------------------------------------------------------------
//                       main
//------------------------------------------------------------------------------------

main(U_INT4 argc, S_CHAR_P argv[])
{
    if ((argc != 1) && (argc != 2)) usage(argv[0]); // 引数のチェック

    string DataPath;                                // データのパス
    if (argc == 1){
        DataPath = "./";
    }else{
        DataPath = argv[1];
        DataPath += "/";
    }

    SERVERMAXLEN = 64;                            // 処理できる入力文の長さの最大値
 
    cerr << "DataPath = " << DataPath << endl;

    KKConv kkconv(DataPath);                      // 仮名漢字変換エンジン

    Connection conn;                              // いまのところ rbuf のみ使用 

    while (conn(cin)){                            // １行読み込む

        if (conn.rbuf == ""){
            cerr << "無視: " << conn.rbuf << "\n" << endl;
            continue;
        }

        if (conn.rbuf.substr(0, 1) == "#"){       // コメント
            cerr << "無視: " << conn.rbuf << "\n" << endl;
            continue;
        }

        if (conn.argv(0) == "END"){               // サーバーの終了
            cerr << "命令: " << conn.argv(0) << "\n" << endl;
            break;
        }

        if (conn.argv(0) == "CONVERT"){           // 入力の変換
            cerr << "命令: " << conn.argv(0) << endl;
            W_String senten(conn.argv(1));
            cerr << "  入力: " << senten << endl;
            if (senten.length() > SERVERMAXLEN){
                cerr << "  出力: " << "ERROR Input_too_long\n" << endl;
                cout << "ERROR Input_too_long" << endl; // endl -> flush ?
                continue;
            }
            string result = kkconv.conv(senten);
            cerr << "  出力: " << result << "\n" << endl;
            cout << result << endl;               // endl -> flush ?
            continue;
        }
        if (conn.argv(0)  == "CONVERT_WITH_1ST_BOUNDARY"){ // 最初の境界を指定
            cerr << "命令: " << conn.argv(0) << endl;
            U_INT4 fb = atoi(conn.argv(1));
            cerr << "  引数: " << fb << endl;
            W_String senten(conn.argv(2));
            cerr << "  入力: " << senten << endl;
            string result = kkconv.conv(senten, fb);
            cerr << "  出力: " << result << "\n" << endl;
            cout << result << endl;
            continue;
        }
        if (conn.argv(0)  == "LIST_CANDIDATE"){
            cerr << "命令: " << conn.argv(0) << endl;
            W_String senten(conn.argv(1));
            cerr << "  入力: " << senten << endl;
            string result = kkconv.list(senten);
            cerr << "  出力: " << result << "\n" << endl;
            cout << result << endl;
            continue;
        }
        if (conn.argv(0)  == "LIST_CANDIDATE_WITH_HISTORY"){
            cerr << "命令: " << conn.argv(0) << endl;
            string prev = conn.argv(1);
            cerr << "  引数: " << prev << endl;
            W_String senten(conn.argv(2));
            cerr << "  入力: " << senten << endl;
            string result = kkconv.list(senten, prev);
            cerr << "  出力: " << result << "\n" << endl;
            cout << result << endl;
            continue;
        }
        if (conn.argv(0)  == "CHANGE_LM_WEIGHT"){
            cerr << "命令: " << conn.argv(0) << endl;
            DECIM8 L = atof(conn.argv(1));
            cerr << "  重み: " << L << endl;
            cerr << "Not Implemented!" << "\n" << endl;
            continue;
        }
    }

    cerr << "Done" << endl;
}


//------------------------------------------------------------------------------------
//                       usage
//------------------------------------------------------------------------------------

void usage(S_CHAR string[])
{
    cerr << "Usage: " << string << endl;
    exit(-1);
}

    
//====================================================================================
//                       END
//====================================================================================
