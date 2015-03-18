//====================================================================================
//                       main.cc
//                            by Shinsuke MORI
//                            Last change : 4 March 1995
//====================================================================================

// ��  ǽ : ������ 2-gram ��ǥ�ˤ�벾̾�����Ѵ���
//
// ����ˡ : main < (filename)
//
// ��  �� : main < ../../corpus/EDR10.senten
//
// ����� : �ʤ�


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#define EXDICT                                    // ������������

#define AC
#define MEMORY

#ifdef DEBUG
#define MAIN_DEBUG
#endif
//#define MAIN_DEBUG                                // ��������η�̤�ɽ��


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
    if ((argc != 1) && (argc != 2)) usage(argv[0]); // �����Υ����å�

    string DataPath;                                // �ǡ����Υѥ�
    if (argc == 1){
        DataPath = "./";
    }else{
        DataPath = argv[1];
        DataPath += "/";
    }

    SERVERMAXLEN = 64;                            // �����Ǥ�������ʸ��Ĺ���κ�����
 
    cerr << "DataPath = " << DataPath << endl;

    KKConv kkconv(DataPath);                      // ��̾�����Ѵ����󥸥�

    Connection conn;                              // ���ޤΤȤ��� rbuf �Τ߻��� 

    while (conn(cin)){                            // �����ɤ߹���

        if (conn.rbuf == ""){
            cerr << "̵��: " << conn.rbuf << "\n" << endl;
            continue;
        }

        if (conn.rbuf.substr(0, 1) == "#"){       // ������
            cerr << "̵��: " << conn.rbuf << "\n" << endl;
            continue;
        }

        if (conn.argv(0) == "END"){               // �����С��ν�λ
            cerr << "̿��: " << conn.argv(0) << "\n" << endl;
            break;
        }

        if (conn.argv(0) == "CONVERT"){           // ���Ϥ��Ѵ�
            cerr << "̿��: " << conn.argv(0) << endl;
            W_String senten(conn.argv(1));
            cerr << "  ����: " << senten << endl;
            if (senten.length() > SERVERMAXLEN){
                cerr << "  ����: " << "ERROR Input_too_long\n" << endl;
                cout << "ERROR Input_too_long" << endl; // endl -> flush ?
                continue;
            }
            string result = kkconv.conv(senten);
            cerr << "  ����: " << result << "\n" << endl;
            cout << result << endl;               // endl -> flush ?
            continue;
        }
        if (conn.argv(0)  == "CONVERT_WITH_1ST_BOUNDARY"){ // �ǽ�ζ��������
            cerr << "̿��: " << conn.argv(0) << endl;
            U_INT4 fb = atoi(conn.argv(1));
            cerr << "  ����: " << fb << endl;
            W_String senten(conn.argv(2));
            cerr << "  ����: " << senten << endl;
            string result = kkconv.conv(senten, fb);
            cerr << "  ����: " << result << "\n" << endl;
            cout << result << endl;
            continue;
        }
        if (conn.argv(0)  == "LIST_CANDIDATE"){
            cerr << "̿��: " << conn.argv(0) << endl;
            W_String senten(conn.argv(1));
            cerr << "  ����: " << senten << endl;
            string result = kkconv.list(senten);
            cerr << "  ����: " << result << "\n" << endl;
            cout << result << endl;
            continue;
        }
        if (conn.argv(0)  == "LIST_CANDIDATE_WITH_HISTORY"){
            cerr << "̿��: " << conn.argv(0) << endl;
            string prev = conn.argv(1);
            cerr << "  ����: " << prev << endl;
            W_String senten(conn.argv(2));
            cerr << "  ����: " << senten << endl;
            string result = kkconv.list(senten, prev);
            cerr << "  ����: " << result << "\n" << endl;
            cout << result << endl;
            continue;
        }
        if (conn.argv(0)  == "CHANGE_LM_WEIGHT"){
            cerr << "̿��: " << conn.argv(0) << endl;
            DECIM8 L = atof(conn.argv(1));
            cerr << "  �Ť�: " << L << endl;
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
