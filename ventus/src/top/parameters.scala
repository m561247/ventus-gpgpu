package top

import L2cache.{CacheParameters, InclusiveCacheMicroParameters, InclusiveCacheParameters_lite}
import chisel3.util._

object parameters { //notice log2Ceil(4) returns 2.that is ,n is the total num, not the last idx.
  def num_sm = 2
  val SINGLE_INST: Boolean = false
  val SPIKE_OUTPUT: Boolean = true
  val INST_CNT: Boolean = false
  val INST_CNT_2: Boolean = true
  val MMU_ENABLED: Boolean = false
  def MMU_ASID_WIDTH = 16
  val wid_to_check = 2
  def num_bank = 4                  // # of banks for register file
  def num_collectorUnit = num_warp
  def num_vgpr:Int = 4096
  def num_sgpr:Int = 4096
  def depth_regBank = log2Ceil(num_vgpr/num_bank)
  def regidx_width = 5

  def regext_width = 3

  var num_warp =  8   //max num_warp in a SM

  def num_cluster = 1

  def num_sm_in_cluster = num_sm / num_cluster
  // def depth_warp = if (num_warp == 1) 1 else log2Ceil(num_warp)
  def depth_warp = if (num_warp > num_bank)  log2Ceil(num_warp) else log2Ceil(num_bank) //log2Ceil(num_warp)  512 dma
  // for register file bank access only, in operand collector
  // Calculate the highest slice index, ensuring it does not exceed the actual width of 'wid'
  def widSliceHigh = scala.math.min(log2Ceil(num_bank) - 1, depth_warp - 1)

  var num_thread = 32

  def depth_thread = log2Ceil(num_thread)

  def num_fetch = 2
  Predef.assert((num_fetch & (num_fetch - 1)) == 0, "num_fetch should be power of 2")

  def icache_align = num_fetch * 4

  def num_issue = 1

  def size_ibuffer = 2

  def xLen = 32 // data length 32-bit

  def instLen = 32

  def addrLen = 32

  def num_block = 8// not bigger than num_warp

  def num_warp_in_a_block = num_warp  //max warp in a block

  def num_lane = num_thread // 2

  def num_icachebuf = 1 //blocking for each warp

  def depth_icachebuf = log2Ceil(num_icachebuf)

  def num_ibuffer = 2

  def depth_ibuffer = log2Ceil(num_ibuffer)

  def lsu_num_entry_each_warp = 4 //blocking for each warp

  def lsu_nMshrEntry = num_warp // less than num_warp

  def dcache_NSets: Int = 256

  def dcache_NWays: Int = 2

  def dcache_BlockWords: Int = 32  // number of words per cacheline(block)
  def dcache_wshr_entry: Int = 4

  def dcache_SetIdxBits: Int = log2Ceil(dcache_NSets)

  def BytesOfWord = 32 / 8

  def dcache_WordOffsetBits = log2Ceil(BytesOfWord) //a Word has 4 Bytes

  def dcache_BlockOffsetBits = log2Ceil(dcache_BlockWords) // select word in block

  def dcache_TagBits = xLen - (dcache_SetIdxBits + dcache_BlockOffsetBits + dcache_WordOffsetBits)

  def dcache_MshrEntry: Int = 4

  def dcache_MshrSubEntry: Int = 2
  def num_sfu = (num_thread >> 2).max(1)

  def sharedmem_depth = 1024

  def sharedmem_BlockWords = dcache_BlockWords

  def sharemem_size = sharedmem_depth * sharedmem_BlockWords * 4 //bytes

  def l2cache_NSets: Int = 64

  def l2cache_NWays: Int = 16

  def l2cache_BlockWords: Int = dcache_BlockWords

  def l2cache_writeBytes: Int = 1

  def l2cache_memCycles: Int = 32

  def l2cache_portFactor: Int = 2

  def l1cache_sourceBits: Int = 3+log2Up(dcache_MshrEntry)+log2Up(dcache_NSets)

  def l2cache_cache = CacheParameters(2, l2cache_NWays, l2cache_NSets, num_l2cache, l2cache_BlockWords << 2, l2cache_BlockWords << 2)
  def l2cache_micro = InclusiveCacheMicroParameters(l2cache_writeBytes, l2cache_memCycles, l2cache_portFactor, num_warp, num_sm, num_sm_in_cluster, num_cluster,dcache_MshrEntry,dcache_NSets)
  def l2cache_micro_l = InclusiveCacheMicroParameters(l2cache_writeBytes, l2cache_memCycles, l2cache_portFactor, num_warp, num_sm, num_sm_in_cluster, 1,dcache_MshrEntry,dcache_NSets)
  def l2cache_params = InclusiveCacheParameters_lite(l2cache_cache, l2cache_micro, false)
  def l2cache_params_l = InclusiveCacheParameters_lite(l2cache_cache, l2cache_micro_l, false)

  def tc_dim: Seq[Int] = {
    var x: Seq[Int] = Seq(2, 2, 2)
    if (num_thread == 8)
      x = Seq(2, 4, 2)
    else if (num_thread == 32)
      x = Seq(4, 8, 4)
    x
  }

  def sig_length = 33

  def num_cache_in_sm = 3  //518 dma 

  def num_l2cache = 1

  val LDS_BASE : BigInt = 0x70000000  // LDS base address: a hyperparameter used within each SM

  def NUMBER_CU = num_sm
  def NUMBER_VGPR_SLOTS = num_vgpr
  def NUMBER_SGPR_SLOTS = num_sgpr
  def NUMBER_LDS_SLOTS = sharemem_size //TODO:check LDS max value. 128kB -> 2^17
  def WG_ID_WIDTH = 32
  def WF_COUNT_MAX = num_warp // max num of wf in a cu
  def WF_COUNT_PER_WG_MAX = num_warp_in_a_block // max num of wf in a wg
  def WAVE_ITEM_WIDTH = 10
  def MEM_ADDR_WIDTH = 32
  def CU_ID_WIDTH = log2Ceil(NUMBER_CU)
  def VGPR_ID_WIDTH = log2Ceil(NUMBER_VGPR_SLOTS)
  def SGPR_ID_WIDTH = log2Ceil(NUMBER_SGPR_SLOTS)
  def LDS_ID_WIDTH = log2Ceil(NUMBER_LDS_SLOTS)
  def WF_COUNT_WIDTH = log2Ceil(WF_COUNT_MAX + 1)
  def WF_COUNT_WIDTH_PER_WG = log2Ceil(WF_COUNT_PER_WG_MAX + 1)
  def TAG_WIDTH = CTA_SCHE_CONFIG.WG.WF_TAG_WIDTH_UINT
  def NUM_WG_DIM = 1024
  def NUM_WG_X = NUM_WG_DIM // max wg num in kernel
  def NUM_WG_Y = NUM_WG_DIM
  def NUM_WG_Z = NUM_WG_DIM
  def WG_SIZE_X_WIDTH = log2Ceil(NUM_WG_X + 1)
  def WG_SIZE_Y_WIDTH = log2Ceil(NUM_WG_Y + 1)
  def WG_SIZE_Z_WIDTH = log2Ceil(NUM_WG_Z + 1)

  object CTA_SCHE_CONFIG {
    import chisel3._
    object GPU {
      val NUM_CU = num_sm
      val MEM_ADDR_WIDTH = parameters.MEM_ADDR_WIDTH.W
      val NUM_WG_SLOT = num_block                  // Number of WG slot in each CU
      val NUM_WF_SLOT = num_warp                   // Number of WF slot in each CU
      val NUM_THREAD = num_thread                  // Number of thread in each WF
      val MMU_ENABLE = MMU_ENABLED                 // if MMU will be used
      val ASID_WIDTH = MMU_ASID_WIDTH.W            // MMU ASID width
    }
    object WG {
      val WG_ID_WIDTH = parameters.WG_ID_WIDTH.W
      val NUM_WG_DIM_MAX = NUM_WG_DIM              // Max number of wg in a single dimension in each kernel
      val NUM_THREAD_MAX = GPU.NUM_THREAD          // Max number of thread in each wavefront(warp)
      val NUM_WF_MAX = num_warp_in_a_block         // Max number of wavefront in each workgroup(block)
      val NUM_LDS_MAX = sharemem_size              // Max number of LDS  occupied by a workgroup
      val NUM_SGPR_MAX = num_sgpr                  // Max number of sgpr occupied by a workgroup
      val NUM_VGPR_MAX = num_vgpr                  // Max number of vgpr occupied by a workgroup
      val NUM_PDS_MAX = 1024*num_thread*2          // Max number of PDS  occupied by a *wavefront*
      //val NUM_GDS_MAX = 1024                     // Max number of GDS  occupied by a workgroup, useless

      // WF tag = cat(wg_slot_id_in_cu, wf_id_in_wg)
      val WF_TAG_WIDTH_UINT = log2Ceil(GPU.NUM_WG_SLOT) + log2Ceil(NUM_WF_MAX)
      val WF_TAG_WIDTH = WF_TAG_WIDTH_UINT.W
    }
    object WG_BUFFER {
      val NUM_ENTRIES = 8
    }
    object RESOURCE_TABLE {
      val NUM_RESULT = 2
    }
    val DEBUG = true
  }

  //518 dma-xrn
  def BitsOfByte = 8

  def maxcopysize = 16 // bytes

  def shared_aligned = 4 //bytes

  var shared_aligned_bits = shared_aligned * 8 //bytes

  def dma_aligned_bulk = 4 // bytes

  var dma_aligned_bulk_bits = dma_aligned_bulk * 8 // bytes

  def max_dma_inst = if(num_warp >= 4) num_warp else 4

  def max_dma_tag = 8

  def max_l2cacheline = 6

  def cacheline = dcache_BlockWords * 4 //number_thread*4= 16*4(word_size=4 Byte)=64 // Math.pow(2, l2cachetagbits << 2).toInt //bytes

  var l2cacheline = cacheline   //number_thread*4= 16*4(word_size=4 Byte)=64
  var sharedcacheline = cacheline

  def l2wayBits = log2Ceil(l2cache_NWays) //4

  def l2setBits = log2Ceil(l2cache_NSets) //6

  def l2offsetBits = log2Ceil(l2cache_BlockWords << 2) // 7

  def l2cBits = log2Ceil(num_l2cache) //1

  var l2cachetagbits = xLen - (l2setBits + l2offsetBits + l2cBits)
  //  var l2cachetagbits = xLen - (l2wayBits + l2setBits + l2offsetBits + l2cBits)
  // 32 - (4 + 5 + 7) = 16
  var l2cachesetbits = l2setBits

  //  var sharedsetbits = dcache_SetIdxBits
  def addr_tag_bits = xLen - log2Ceil(l2cacheline)
  def addr_set_bits = xLen - log2Ceil(sharedcacheline)

  def numgroupl2cache = l2cacheline / dma_aligned_bulk  // (number_thread*4= 16*4(word_size=4 Byte) )  / 4  =64 /4 =16

  def numgroupshared = num_thread * 4 / dma_aligned_bulk  // (number_thread*4= 16*4(word_size=4 Byte) )  / 4  =64 /4 =16

  def numgroupinsdmax = maxcopysize / dma_aligned_bulk

  def warp_align_async = if(num_warp >= 4) 4 else 1

  def num_wgroup = num_warp/warp_align_async



  def Binary_   = 0// 00000                 
  def INT4      = 1// 00001        
  def FP4_E3M0  = 2// 00010            
  def FP4_E2M1  = 3// 00011                 
  def FP6_E3M2  = 4// 00100            
  def FP6_E2M3  = 5// 00101                 
  def INT8      = 6// 00110        
  def UINT8     = 7// 00111         
  def FP8_E4M3  = 8// 01000            
  def FP8_E5M2  = 9// 01001   

  def INT16     = 10// 01010         
  def UINT16    = 11// 01011          
  def FP16      = 12// 01100         
  def BF16      = 13// 01101  

  def INT32     = 14// 01110         
  def UINT32    = 15// 01111          
  def FP32      = 16// 10000        
  def FP32_FTZ  = 17// 10001                 
  def TF32      = 18// 10010                 
  def TF32_FTZ  = 19// 10011   

  def INT64     = 20// 10100         
  def UINT64    = 21// 10101          
  def FP64      = 22// 10110

  def paLen = 34

}
