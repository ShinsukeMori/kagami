//====================================================================================
//                       StopChar.h
//                            by Shinsuke MORI
//                            Last change : 15 November 1995
//====================================================================================

// ��  ǽ : ñ��ʬ��ˤ����ƾ�˰�ñ��Ȥ��ư�����ʸ���Υ��饹
//
// ��  �� : �ʤ�


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _StopChar_h
#define _StopChar_h 1
//#define StopChar_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <mystd.h>
#include <W_CHAR.h>

#include "StopChar.h"


//------------------------------------------------------------------------------------
//                       class StopChar
//------------------------------------------------------------------------------------

class StopChar{

  public:

         StopChar(const string& filestem);

    BOOL operator[](W_CHAR wc){return(flag[wc.full]);};

    BOOL operator[](U_INT2 wc){return(flag[wc]);};

  protected:

  private:

    BOOL flag[U_INT2_MAX+1];                      // W_CHAR -> BOOL

};


//------------------------------------------------------------------------------------
//                       StopChar
//------------------------------------------------------------------------------------

StopChar::StopChar(const string& filestem)
{
#ifdef StopChar_DEBUG
    cerr << "StopChar::StopChar(const string&)" << endl;
#endif

    cerr << memalloced(sizeof(flag)) << " for " << filestem + ".text" << endl;

    for (U_INT4 i = 0; i < U_INT2_MAX+1; i++){    // �����Ǥ�ֵ��פǽ����
        flag[i] = FALSE;
    }

    ifstream file((filestem + ".text").c_str());
    if (! file) openfailed(filestem + ".text");

    for (S_CHAR line[3]; file.getline(line, 3); ){// �ե�����γƹԤ�ʸ���Υ롼��
        flag[W_CHAR(line).full] = TRUE;           // ʸ�����ͤ�ֿ��פ��ѹ�
    }

    file.close();

#ifdef StopChar_DEBUG
    cerr << "StopChar = ";
    for (U_INT4 wc = 0; wc < U_INT2_MAX+1; wc++){
        if (flag[wc] == TRUE){
            cerr << W_CHAR(wc);
        }
    }
    cerr << endl;
#endif
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
