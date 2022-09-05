{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnNetDecls;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�����ͨѶ���������ṹ���嵥Ԫ
* ��Ԫ���ߣ�CnPack������ Liu Xiao
* ��    ע��
* ����ƽ̨��PWinXP + Delphi XE
* ���ݲ��ԣ�PWinXP/7 + Delphi 2009 ~
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2016.10.05 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows;

const
  {* IP ��ͷ�еİ汾�ֶεĶ���}
  CN_IP_VERSION_V4                          = 4;
  CN_IP_VERSION_V6                          = 6;

  {* IP ��ͷ�� Type of Service �ֶ��е� Precedence ����}
  CN_IP_TOS_PRECEDENCE_ROUTINE              = 0;
  CN_IP_TOS_PRECEDENCE_PRIORITY             = 1;
  CN_IP_TOS_PRECEDENCE_IMMEDIATE            = 2;
  CN_IP_TOS_PRECEDENCE_FLASH                = 3;
  CN_IP_TOS_PRECEDENCE_FLASH_OVERRIDE       = 4;
  CN_IP_TOS_PRECEDENCE_CRITIC_ECP           = 5;
  CN_IP_TOS_PRECEDENCE_INTERNETWORK_CONTROL = 6;
  CN_IP_TOS_PRECEDENCE_NETWORK_CONTROL      = 7;

  {* IP ��ͷ�� Type of Service �ֶ��е������������ͱ�Ƕ���}
  CN_IP_TOS_DELAY_MASK                      = $10;
  CN_IP_TOS_THROUGHPUT_MASK                 = $8;
  CN_IP_TOS_RELIBILITY_MASK                 = $4;

  {* IP ��ͷ�� Fragment Flag �ֶ��еķ�Ƭ��Ƕ���}
  CN_IP_FLAG_DONT_FRAGMENT_WORD_MASK        = $4000;
  CN_IP_FLAG_MORE_FRAGMENT_WORD_MASK        = $2000;
  CN_IP_FLAG_FRAGMENT_OFFSET_WORD_MASK      = $1FFF;

  {* IP ��ͷ��Э���ֶεĶ��壬�ο���ά���ٿ�}
  CN_IP_PROTOCOL_HOPOPT          = $00; // IPv6 Hop-by-Hop Option
  CN_IP_PROTOCOL_ICMP            = $01; // *** Internet Control Message Protocol
  CN_IP_PROTOCOL_IGMP            = $02; // *** Internet Group Management Protocol
  CN_IP_PROTOCOL_GGP             = $03; // Gateway-to-Gateway Protocol
  CN_IP_PROTOCOL_IP_IN_IP        = $04; // IP in IP?(encapsulation)
  CN_IP_PROTOCOL_ST              = $05; // Internet Stream Protocol
  CN_IP_PROTOCOL_TCP             = $06; // *** Transmission Control Protocol
  CN_IP_PROTOCOL_CBT             = $07; // Core-based trees
  CN_IP_PROTOCOL_EGP             = $08; // Exterior Gateway Protocol
  CN_IP_PROTOCOL_IGP             = $09; // Interior Gateway Protocol
  CN_IP_PROTOCOL_BBN_RCC_MON     = $0A; // BBN RCC Monitoring
  CN_IP_PROTOCOL_NVP_II          = $0B; // Network Voice Protocol
  CN_IP_PROTOCOL_PUP             = $0C; // Xerox PUP
  CN_IP_PROTOCOL_ARGUS           = $0D; // ARGUS
  CN_IP_PROTOCOL_EMCON           = $0E; // EMCON
  CN_IP_PROTOCOL_XNET            = $0F; // Cross Net Debugger
  CN_IP_PROTOCOL_CHAOS           = $10; // Chaos
  CN_IP_PROTOCOL_UDP             = $11; // *** User Datagram Protocol
  CN_IP_PROTOCOL_MUX             = $12; // Multiplexing
  CN_IP_PROTOCOL_DCN_MEAS        = $13; // DCN Measurement Subsystems
  CN_IP_PROTOCOL_HMP             = $14; // Host Monitoring Protocol
  CN_IP_PROTOCOL_PRM             = $15; // Packet Radio Measurement
  CN_IP_PROTOCOL_XNS_IDP         = $16; // XEROX NS IDP
  CN_IP_PROTOCOL_TRUNK_1         = $17; // Trunk-1
  CN_IP_PROTOCOL_TRUNK_2         = $18; // Trunk-2
  CN_IP_PROTOCOL_LEAF_1          = $19; // Leaf-1
  CN_IP_PROTOCOL_LEAF_2          = $1A; // Leaf-2
  CN_IP_PROTOCOL_RDP             = $1B; // Reliable Datagram Protocol
  CN_IP_PROTOCOL_IRTP            = $1C; // Internet Reliable Transaction Protocol
  CN_IP_PROTOCOL_ISO_TP4         = $1D; // ISO Transport Protocol Class 4
  CN_IP_PROTOCOL_NETBLT          = $1E; // Bulk Data Transfer Protocol
  CN_IP_PROTOCOL_MFE_NSP         = $1F; // MFE Network Services Protocol
  CN_IP_PROTOCOL_MERIT_INP       = $20; // MERIT Internodal Protocol
  CN_IP_PROTOCOL_DCCP            = $21; // Datagram Congestion Control Protocol
  CN_IP_PROTOCOL_3PC             = $22; // Third Party Connect Protocol
  CN_IP_PROTOCOL_IDPR            = $23; // Inter-Domain Policy Routing Protocol
  CN_IP_PROTOCOL_XTP             = $24; // Xpress Transport Protocol
  CN_IP_PROTOCOL_DDP             = $25; // Datagram Delivery Protocol
  CN_IP_PROTOCOL_IDPR_CMTP       = $26; // IDPR Control Message Transport Protocol
  CN_IP_PROTOCOL_TP_PP           = $27; // TP++ Transport Protocol
  CN_IP_PROTOCOL_IL              = $28; // IL Transport Protocol
  CN_IP_PROTOCOL_IPV6            = $29; // IPv6 Encapsulation
  CN_IP_PROTOCOL_SDRP            = $2A; // Source Demand Routing Protocol
  CN_IP_PROTOCOL_IPV6_ROUTE      = $2B; // Routing Header for?IPv6
  CN_IP_PROTOCOL_IPV6_FRAG       = $2C; // Fragment Header for?IPv6
  CN_IP_PROTOCOL_IDRP            = $2D; // Inter-Domain Routing Protocol
  CN_IP_PROTOCOL_RSVP            = $2E; // Resource Reservation Protocol
  CN_IP_PROTOCOL_GRE             = $2F; // Generic Routing Encapsulation
  CN_IP_PROTOCOL_MHRP            = $30; // Mobile Host Routing Protocol
  CN_IP_PROTOCOL_BNA             = $31; // BNA
  CN_IP_PROTOCOL_ESP             = $32; // Encapsulating Security Payload
  CN_IP_PROTOCOL_AH              = $33; // Authentication Header
  CN_IP_PROTOCOL_I_NLSP          = $34; // Integrated Net Layer Security Protocol
  CN_IP_PROTOCOL_SWIPE           = $35; // SwIPe
  CN_IP_PROTOCOL_NARP            = $36; // NBMA Address Resolution Protocol
  CN_IP_PROTOCOL_MOBILE          = $37; // IP Mobility?(Min Encap)
  CN_IP_PROTOCOL_TLSP            = $38; // Transport Layer Security Protocol
  CN_IP_PROTOCOL_SKIP            = $39; // Simple Key-Management for Internet Protocol
  CN_IP_PROTOCOL_IPV6_ICMP       = $3A; // ICMP for IPv6
  CN_IP_PROTOCOL_IPV6_NONXT      = $3B; // No Next Header for?IPv6
  CN_IP_PROTOCOL_IPV6_OPTS       = $3C; // Destination Options for?IPv6
  CN_IP_PROTOCOL_ANY_HOST        = $3D; // Any host internal protocol
  CN_IP_PROTOCOL_CFTP            = $3E; // CFTP
  CN_IP_PROTOCOL_ANY_LOCAL       = $3F; // Any local network
  CN_IP_PROTOCOL_SAT_EXPAK       = $40; // SATNET and Backroom EXPAK
  CN_IP_PROTOCOL_KRYPTOLAN       = $41; // Kryptolan
  CN_IP_PROTOCOL_RVD             = $42; // MIT?Remote Virtual Disk Protocol
  CN_IP_PROTOCOL_IPPC            = $43; // Internet Pluribus Packet Core
  CN_IP_PROTOCOL_ANY_DFS         = $44; // Any distributed file system
  CN_IP_PROTOCOL_SAT_MON         = $45; // SATNET Monitoring
  CN_IP_PROTOCOL_VISA            = $46; // VISA Protocol
  CN_IP_PROTOCOL_IPCU            = $47; // Internet Packet Core Utility
  CN_IP_PROTOCOL_CPNX            = $48; // Computer Protocol Network Executive
  CN_IP_PROTOCOL_CPHB            = $49; // Computer Protocol Heart Beat
  CN_IP_PROTOCOL_WSN             = $4A; // Wang Span Network
  CN_IP_PROTOCOL_PVP             = $4B; // Packet Video Protocol
  CN_IP_PROTOCOL_BR_SAT_MON      = $4C; // Backroom SATNET Monitoring
  CN_IP_PROTOCOL_SUN_ND          = $4D; // SUN ND PROTOCOL-Temporary
  CN_IP_PROTOCOL_WB_MON          = $4E; // WIDEBAND Monitoring
  CN_IP_PROTOCOL_WB_EXPAK        = $4F; // WIDEBAND EXPAK
  CN_IP_PROTOCOL_ISO_IP          = $50; // International Organization for Standardization Internet Protocol
  CN_IP_PROTOCOL_VMTP            = $51; // Versatile Message Transaction Protocol
  CN_IP_PROTOCOL_SECURE_VMTP     = $52; // Secure Versatile Message Transaction Protocol
  CN_IP_PROTOCOL_VINES           = $53; // VINES
  CN_IP_PROTOCOL_TTP             = $54; // TTP
  CN_IP_PROTOCOL_IPTM            = $54; // Internet Protocol Traffic Manager
  CN_IP_PROTOCOL_NSFNET_IGP      = $55; // NSFNET-IGP
  CN_IP_PROTOCOL_DGP             = $56; // Dissimilar Gateway Protocol
  CN_IP_PROTOCOL_TCF             = $57; // TCF
  CN_IP_PROTOCOL_EIGRP           = $58; // EIGRP
  CN_IP_PROTOCOL_OSPF            = $59; // Open Shortest Path First
  CN_IP_PROTOCOL_SPRITE_RPC      = $5A; // Sprite RPC Protocol
  CN_IP_PROTOCOL_LARP            = $5B; // Locus Address Resolution Protocol
  CN_IP_PROTOCOL_MTP             = $5C; // Multicast Transport Protocol
  CN_IP_PROTOCOL_AX_25           = $5D; // AX.25
  CN_IP_PROTOCOL_OS              = $5E; // KA9Q NOS compatible IP over IP tunneling
  CN_IP_PROTOCOL_MICP            = $5F; // Mobile Internetworking Control Protocol
  CN_IP_PROTOCOL_SCC_SP          = $60; // Semaphore Communications Sec. Pro
  CN_IP_PROTOCOL_ETHERIP         = $61; // Ethernet-within-IP Encapsulation
  CN_IP_PROTOCOL_ENCAP           = $62; // Encapsulation Header
  CN_IP_PROTOCOL_ANY_PRIVATE     = $63; // Any private encryption scheme
  CN_IP_PROTOCOL_GMTP            = $64; // GMTP
  CN_IP_PROTOCOL_IFMP            = $65; // Ipsilon Flow Management Protocol
  CN_IP_PROTOCOL_PNNI            = $66; // PNNI over IP
  CN_IP_PROTOCOL_PIM             = $67; // Protocol Independent Multicast
  CN_IP_PROTOCOL_ARIS            = $68; // IBM's ARIS (Aggregate Route IP Switching) Protocol
  CN_IP_PROTOCOL_SCPS            = $69; // SCPS (Space Communications Protocol Standards)
  CN_IP_PROTOCOL_QNX             = $6A; // QNX
  CN_IP_PROTOCOL_A_N             = $6B; // Active Networks
  CN_IP_PROTOCOL_IPCOMP          = $6C; // IP Payload Compression Protocol
  CN_IP_PROTOCOL_SNP             = $6D; // Sitara Networks Protocol
  CN_IP_PROTOCOL_COMPAQ_PEER     = $6E; // Compaq Peer Protocol
  CN_IP_PROTOCOL_IPX_IN_IP       = $6F; // IPX in IP
  CN_IP_PROTOCOL_VRRP            = $70; // Virtual Router Redundancy Protocol,?Common Address Redundancy Protocol?(not?IANAassigned)
  CN_IP_PROTOCOL_PGM             = $71; // PGM Reliable Transport Protocol
  CN_IP_PROTOCOL_ANY_0HOP        = $72; // Any 0-hop protocol
  CN_IP_PROTOCOL_L2TP            = $73; // Layer Two Tunneling Protocol Version 3
  CN_IP_PROTOCOL_DDX             = $74; // D-II Data Exchange (DDX)
  CN_IP_PROTOCOL_IATP            = $75; // Interactive Agent Transfer Protocol
  CN_IP_PROTOCOL_STP             = $76; // Schedule Transfer Protocol
  CN_IP_PROTOCOL_SRP             = $77; // SpectraLink Radio Protocol
  CN_IP_PROTOCOL_UTI             = $78; // Universal Transport Interface Protocol
  CN_IP_PROTOCOL_SMP             = $79; // Simple Message Protocol
  CN_IP_PROTOCOL_SM              = $7A; // Simple Multicast Protocol
  CN_IP_PROTOCOL_PTP             = $7B; // Performance Transparency Protocol
  CN_IP_PROTOCOL_IS_IS_OVER_IPV4 = $7C; // Intermediate System to Intermediate System (IS-IS) Protocol?over?IPv4
  CN_IP_PROTOCOL_FIRE            = $7D; // Flexible Intra-AS Routing Environment
  CN_IP_PROTOCOL_CRTP            = $7E; // Combat Radio Transport Protocol
  CN_IP_PROTOCOL_CRUDP           = $7F; // Combat Radio User Datagram
  CN_IP_PROTOCOL_SSCOPMCE        = $80; // Service-Specific Connection-Oriented Protocol in a Multilink and Connectionless Environment
  CN_IP_PROTOCOL_IPLT            = $81; // IPLT
  CN_IP_PROTOCOL_SPS             = $82; // Secure Packet Shield
  CN_IP_PROTOCOL_PIPE            = $83; // Private IP Encapsulation within IP
  CN_IP_PROTOCOL_SCTP            = $84; // Stream Control Transmission Protocol
  CN_IP_PROTOCOL_FC              = $85; // Fibre Channel
  CN_IP_PROTOCOL_RSVP_E2E_IGNORE = $86; // Reservation Protocol (RSVP) End-to-End Ignore                                              
  CN_IP_PROTOCOL_MOBILITY_HEADER = $87; // Mobility Extension Header for IPv6                                                         
  CN_IP_PROTOCOL_UDPLITE         = $88; // Lightweight User Datagram Protocol
  CN_IP_PROTOCOL_MPLS_IN_IP      = $89; // Multiprotocol Label Switching?Encapsulated in IP
  CN_IP_PROTOCOL_MANET           = $8A; // MANET?Protocols
  CN_IP_PROTOCOL_HIP             = $8B; // Host Identity Protocol
  CN_IP_PROTOCOL_SHIM6           = $8C; // Site Multihoming by IPv6 Intermediation
  CN_IP_PROTOCOL_WESP            = $8D; // Wrapped Encapsulating Security Payload
  CN_IP_PROTOCOL_ROHC            = $8E; // Robust Header Compression
  CN_IP_PROTOCOL_RESERVE         = $FF; // Reserved

  {* TCP ��ͷ�еı���ֶεĶ���}
  CN_TCP_FLAG_URG_MASK = $20;
  CN_TCP_FLAG_ACK_MASK = $10;
  CN_TCP_FLAG_PSH_MASK = $8;
  CN_TCP_FLAG_RST_MASK = $4;
  CN_TCP_FLAG_SYN_MASK = $2;
  CN_TCP_FLAG_FIN_MASK = $1;

  {* ICMP ��ͷ�е���Ϣ���Ͷ���}
  CN_ICMP_TYPE_ECHO_REPLY                   = 0;
  CN_ICMP_TYPE_DESTINATION_UNREACHABLE      = 3;
  CN_ICMP_TYPE_SOURCE_QUENCH                = 4;
  CN_ICMP_TYPE_REDIRECT                     = 5;
  CN_ICMP_TYPE_ALTERNATE_HOST_ADDRESS       = 6;
  CN_ICMP_TYPE_ECHO                         = 8;
  CN_ICMP_TYPE_ROUTER_ADVERTISEMENT         = 9;
  CN_ICMP_TYPE_ROUTER_SOLICITATION          = 10;
  CN_ICMP_TYPE_TIME_EXCEEDED                = 11;
  CN_ICMP_TYPE_PARAMETER_PROBLEM            = 12;
  CN_ICMP_TYPE_TIMESTAMP                    = 13;
  CN_ICMP_TYPE_TIMESTAMP_REPLY              = 14;
  CN_ICMP_TYPE_INFORMATION_REQUEST          = 15;
  CN_ICMP_TYPE_INFORMATION_REPLY            = 16;
  CN_ICMP_TYPE_ADDRESS_MASK_REQUEST         = 17;
  CN_ICMP_TYPE_ADDRESS_MASK_REPLY           = 18;
  CN_ICMP_TYPE_TRACEROUTE                   = 30;
  CN_ICMP_TYPE_DATAGRAM_CONVERSION_ERROR    = 31;
  CN_ICMP_TYPE_MOBILE_HOST_REDIRECT         = 32;
  CN_ICMP_TYPE_IPV6_WHERE_ARE_YOU           = 33;
  CN_ICMP_TYPE_IPV6_I_AM_HERE               = 34;
  CN_ICMP_TYPE_MOBILE_REGISTRATION_REQUEST  = 35;
  CN_ICMP_TYPE_MOBILE_REGISTRATION_REPLY    = 36;
  CN_ICMP_TYPE_DOMAIN_NAME_REQUEST          = 37;
  CN_ICMP_TYPE_DOMAIN_NAME_REPLY            = 38;
  CN_ICMP_TYPE_SKIP                         = 39;
  CN_ICMP_TYPE_PHOTURIS                     = 40;
  CN_ICMP_TYPE_UTILIZED_BY_MOBILITY         = 41;

  {* ICMP ��ͷ�е���Ϣ���붨��}
  CN_ICMP_CODE_NO_CODE                      = 0;

  // Ŀ�Ĳ��ɴ����� CN_ICMP_TYPE_DESTINATION_UNREACHABLE
  CN_ICMP_CODE_NET_UNREACHABLE              = 0;   // Net Unreachable
  CN_ICMP_CODE_HOST_UNREACHABLE             = 1;   // Host Unreachable
  CN_ICMP_CODE_PROTOCOL_UNREACHABLE         = 2;   // Protocol Unreachable
  CN_ICMP_CODE_PORT_UNREACHABLE             = 3;   // Port Unreachable
  CN_ICMP_CODE_FRAGMENTATION_NEEDED         = 4;   // Fragmentation Needed and Don't Fragment was Set
  CN_ICMP_CODE_SOURCE_ROUTE_FAILED          = 5;   // Source Route Failed
  CN_ICMP_CODE_DEST_NETWORK_UNKNOWN         = 6;   // Destination Network Unknown
  CN_ICMP_CODE_DEST_HOST_UNKNOWN            = 7;   // Destination Host Unknown
  CN_ICMP_CODE_SOURCE_HOST_ISOLATED         = 8;   // Source Host Isolated
  CN_ICMP_CODE_NETWORK_PROHIBITED           = 9;   // Communication with Destination Network is Administratively Prohibited
  CN_ICMP_CODE_HOST_PROHIBITED              = 10;  // Communication with Destination Host is Administratively Prohibited
  CN_ICMP_CODE_NETWORK_UNREACHABLE_FOR_TOS  = 11;  // Destination Network Unreachable for Type of Service
  CN_ICMP_CODE_HOST_UNREACHABLE_FOR_TOS     = 12;  // Destination Host Unreachable for Type of Service
  CN_ICMP_CODE_COMMUNICATION_PROHIBITED     = 13;  // Communication Administratively Prohibited
  CN_ICMP_CODE_HOST_PRECEDENCE_VIOLATION    = 14;  // Host Precedence Violation
  CN_ICMP_CODE_PRECEDENCE_CUTOFF_IN_EFFECT  = 15;  // Precedence cutoff in effect

  // �ض������� CN_ICMP_TYPE_REDIRECT
  CN_ICMP_CODE_REDIRECT_FOR_NETWORK         = 0;
  CN_ICMP_CODE_REDIRECT_FOR_HOST            = 1;
  CN_ICMP_CODE_REDIRECT_FOR_TOS_NETWORK     = 2;
  CN_ICMP_CODE_REDIRECT_FOR_TOS_HOST        = 3;

  // ����������ַ���� CN_ICMP_TYPE_ALTERNATE_HOST_ADDRESS
  CN_ICMP_CODE_ALTERNATE_ADDRESS_FOR_HOST   = 0;

  // ·�ɹ������� CN_ICMP_TYPE_ROUTER_ADVERTISEMENT
  CN_ICMP_CODE_NORMAL_ROUTER_ADVERTISEMENT  = 0;
  CN_ICMP_CODE_NOT_ROUTE_COMMON_TRAFFIC     = 1;

  // ��ʱ���� CN_ICMP_TYPE_TIME_EXCEEDED
  CN_ICMP_CODE_TTL_EXCEEDED_IN_TRANSIT      = 0;
  CN_ICMP_CODE_FRAGMENT_REASSEMBLY          = 1;

  // ������������ CN_ICMP_TYPE_PARAMETER_PROBLEM
  CN_ICMP_CODE_POINTER_INDICATES_THE_ERROR  = 0;
  CN_ICMP_CODE_MISSING_A_REQUIRED_OPTION    = 1;
  CN_ICMP_CODE_BAD_LENGTH                   = 2;

  // Photuris ���� CN_ICMP_TYPE_PHOTURIS
  CN_ICMP_CODE_BAD_SPI                      = 0;
  CN_ICMP_CODE_AUTHENTICATION_FAILED        = 1;
  CN_ICMP_CODE_DECOMPRESSION_FAILED         = 2;
  CN_ICMP_CODE_DECRYPTION_FAILED            = 3;
  CN_ICMP_CODE_NEED_AUTHENTICATION          = 4;
  CN_ICMP_CODE_NEED_AUTHORIZATION           = 5;

  {* NTP ���е������Ƕ���}
  CN_NTP_LEAP_INDICATOR_NONE                = 0;   // ������
  CN_NTP_LEAP_INDICATOR_LEAP_61             = 1;   // ���������� 61
  CN_NTP_LEAP_INDICATOR_LEAP_59             = 2;   // ���������� 59
  CN_NTP_LEAP_INDICATOR_LEAP_NO_SYNC        = 3;   // ʱ��δͬ��

  {* NTP ���еİ汾���ֶ�ֵ}
  CN_NTP_VERSION_V3                         = 3;

  {* NTP ���е�ģʽ�ֶ�ֵ}
  CN_NTP_MODE_UNSPECIFIED                   = 0;   // δָ��
  CN_NTP_MODE_SYMMETRIC_ACTIVE              = 1;   // �Գ�����
  CN_NTP_MODE_SYMMETRIC_PASSIVE             = 2;   // �ԳƱ���
  CN_NTP_MODE_CLIENT                        = 3;   // �ͻ���
  CN_NTP_MODE_SERVER                        = 4;   // ������
  CN_NTP_MODE_BROADCAST                     = 5;   // �㲥
  CN_NTP_MODE_CONTROL_MSG                   = 6;   // NTP ������Ϣ

  {* NTP ���е�ʱ��������С��������΢��Ļ������ֵ}
  CN_NTP_MICRO_SEC_FRACTION                 = 4294.967296;

  {* DNS ��ͷ�е� QR �ֶ�ֵ}
  CN_DNS_HEADER_TYPE_QUERY                  = 0;   // ����
  CN_DNS_HEADER_TYPE_RESPONSE               = 1;   // Ӧ��

  {* DNS ��ͷ�е� OpCode �ֶ�ֵ}
  CN_DNS_HEADER_OPCODE_STANDARD_QUERY       = 0;   // ��׼��ѯ
  CN_DNS_HEADER_OPCODE_INVERSE_QUERY        = 1;   // �����ѯ
  CN_DNS_HEADER_OPCODE_SERVER_STATUS        = 2;   // ������״̬��ѯ

  {* DNS ��ͷ�е� ResponseCode �ֶ�ֵ}
  CN_DNS_HEADER_RCODE_NOERROR               = 0;   // �޴���
  CN_DNS_HEADER_RCODE_FORMAT_ERROR          = 1;   // ���ĸ�ʽ����
  CN_DNS_HEADER_RCODE_SERVER_FAILURE        = 2;   // ������ʧ��
  CN_DNS_HEADER_RCODE_NAME_ERROR            = 3;   // ���ִ���
  CN_DNS_HEADER_RCODE_NOT_IMPLEMENTED       = 4;   // û��ʵ��
  CN_DNS_HEADER_RCODE_REFUSED               = 5;   // �ܾ�

  {* DNS ���е� TYPE �ֶ�ֵ������ QUESTION ���� QTYPE �ֶ�ֵ}
  CN_DNS_TYPE_A                             = 1;   // a host address
  CN_DNS_TYPE_NS                            = 2;   // an authoritative name server
  CN_DNS_TYPE_MD                            = 3;   // a mail destination (Obsolete - use MX)
  CN_DNS_TYPE_MF                            = 4;   // a mail forwarder (Obsolete - use MX)
  CN_DNS_TYPE_CNAME                         = 5;   // the canonical name for an alias
  CN_DNS_TYPE_SOA                           = 6;   // marks the start of a zone of authority
  CN_DNS_TYPE_MB                            = 7;   // a mailbox domain name (EXPERIMENTAL)
  CN_DNS_TYPE_MG                            = 8;   // a mail group member (EXPERIMENTAL)
  CN_DNS_TYPE_MR                            = 9;   // a mail rename domain name (EXPERIMENTAL)
  CN_DNS_TYPE_NULL                          = 10;  // a null RR (EXPERIMENTAL)
  CN_DNS_TYPE_WKS                           = 11;  // a well known service description
  CN_DNS_TYPE_PTR                           = 12;  // a domain name pointer
  CN_DNS_TYPE_HINFO                         = 13;  // host information
  CN_DNS_TYPE_MINFO                         = 14;  // mailbox or mail list information
  CN_DNS_TYPE_MX                            = 15;  // mail exchange
  CN_DNS_TYPE_TXT                           = 16;  // text strings

  CN_DNS_QTYPE_AXFR                         = 252; // A request for a transfer of an entire zone
  CN_DNS_QTYPE_MAILB                        = 253; // A request for mailbox-related records (MB, MG or MR)
  CN_DNS_QTYPE_MAILA                        = 254; // A request for mail agent RRs (Obsolete - see MX)
  CN_DNS_QTYPE_ALL                          = 255; // A request for all records

  {* DNS ���е� CLASS �ֶ�ֵ������ QCLASS �ֶ�ֵ}
  CN_DNS_CLASS_IN                           = 1;   // the Internet
  CN_DNS_CLASS_CS                           = 2;   // the CSNET class (Obsolete)
  CN_DNS_CLASS_CH                           = 3;   // the CHAOS class
  CN_DNS_CLASS_HS                           = 4;   // Hesiod [Dyer 87]
  CN_DNS_QCLASS_ANY                         = 255; // any class

  {* Socks ����Э������ְ��еİ汾�ֶεĶ���}
  CN_SOCKS_VERSION_V4                       = 4;
  CN_SOCKS_VERSION_V5                       = 5;

  {* Socks ����Э������ְ��еķ����ֶεĶ���}
  CN_SOCKS_METHOD_NO_AUTH_REQUIRED          = $00; // ���������֤
  CN_SOCKS_METHOD_GSSAPI                    = $01; // GSSAPI ��֤
  CN_SOCKS_METHOD_USERNAME_PASSWORD         = $02; // �û���������֤
  CN_SOCKS_METHOD_IANA_ASSIGNED_BEGIN       = $03; // IANA ���俪ʼ
  CN_SOCKS_METHOD_IANA_ASSIGNED_END         = $7F; // IANA �������
  CN_SOCKS_METHOD_RESERVED_PRIVATE_BEGIN    = $80; // ˽�б�����ʼ
  CN_SOCKS_METHOD_RESERVED_PRIVATE_END      = $FE; // ˽�б�������
  CN_SOCKS_METHOD_NO_ACCEPTABLE_METHODS     = $FF; // �޿�����֤����

  {* Socks ����Э��������е������ֶεĶ���}
  CN_SOCKS_CMD_CONNECT                      = $01;
  CN_SOCKS_CMD_BIND                         = $02;
  CN_SOCKS_CMD_UDP                          = $03;

  {* Socks ����Э��������еĵ�ַ�����ֶεĶ���}
  CN_SOCKS_ADDRESS_TYPE_IPV4                = $01;
  CN_SOCKS_ADDRESS_TYPE_DOMAINNAME          = $03;
  CN_SOCKS_ADDRESS_TYPE_IPV6                = $04;

  {* Socks ����Э������ְ����û���������֤�а汾�ֶεĶ���}
  CN_SOCKS_USERNAME_PASSWORD_VER            = $01;

  {* Socks ����Э������ְ����û���������֤�н���ֶεĶ���}
  CN_SOCKS_USERNAME_PASSWORD_STATUS_SUCCESS = $00; // �����֤�ɹ�

  {* Socks ����Э���Ӧ����е���Ӧ�ֶεĶ���}
  CN_SOCKS_REPLY_SUCCESS                    = $00; // �ɹ�
  CN_SOCKS_REPLY_GENERAL_FAILURE            = $01; // ����������
  CN_SOCKS_REPLY_NOT_ALLOWED                = $02; // ��������
  CN_SOCKS_REPLY_NETWORK_UNREACHABLE        = $03; // ���粻�ɴ�
  CN_SOCKS_REPLY_HOST_UNREACHABLE           = $04; // �������ɴ�
  CN_SOCKS_REPLY_CONNECTION_REFUSED         = $05; // ���ӱ��ܾ�
  CN_SOCKS_REPLY_TTL_EXPIRED                = $06; // TTL ����
  CN_SOCKS_REPLY_COMMAND_NOT_SUPPORTED      = $07; // ���֧��
  CN_SOCKS_REPLY_ADDRESS_TYPE_NOT_SUPPORTED = $08; // ��ַ���Ͳ�֧��

type

{*
  IP ��ͷʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |Version|  IHL  |Type of Service|          Total Length         |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |         Identification        |Flags|     Fragment Offset     |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |  Time to Live |    Protocol   |         Header Checksum       |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                     Source IP Address                         |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                  Destination IP Address                       |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                    Options                    |    Padding    |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                             Data                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnIPHeader = packed record
    VerionHeaderLength: Byte;           // �汾�Ͱ�ͷ����
    TypeOfService:      Byte;           // ��������
    TotalLength:        Word;           // �ܳ��ȣ���� 65535
    Identification:     Word;           // ��ʶ
    FlagOffset:         Word;           // ��־��Ƭƫ��
    TTL:                Byte;           // ����ʱ��
    Protocol:           Byte;           // Э��
    Checksum:           Word;           // ��ͷУ���
    SourceIp:           LongWord;       // Դ IP ��ַ
    DestIp:             LongWord;       // Ŀ�� IP ��ַ
  end;

  PCnIPHeader = ^TCnIPHeader;

{*
  TCP ��ͷʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |          Source Port          |       Destination Port        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                        Sequence Number                        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                    Acknowledgment Number                      |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |  Data |           |U|A|P|R|S|F|                               |
  | Offset| Reserved  |R|C|S|S|Y|I|            Window             |
  |       |           |G|K|H|T|N|N|                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |           Checksum            |         Urgent Pointer        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                    Options                    |    Padding    |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                             Data                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnTCPHeader = packed record
    SourcePort:            Word;        // Դ�˿�
    DestPort:              Word;        // Ŀ�Ķ˿�
    SequenceNumber:        LongWord;    // ���к�
    AcknowledgementNumber: LongWord;    // ��Ӧ���к�
    Offset:                Byte;        // ����ƫ�ƣ������� 4 bit����ͬ�ڰ�ͷ����
    Flags:                 Byte;        // TCP ��ͷ���
    Window:                Word;        // ���ڴ�С
    Checksum:              Word;        // У���
    UrgentPointer:         Word;        // ����ָ��
  end;

  PCnTCPHeader = ^TCnTCPHeader;

{*
  UDP ��ͷʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |          Source Port          |       Destination Port        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |             Length            |            Checksum           |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                             Data                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnUDPHeader = packed record
    SourcePort:            Word;        // Դ�˿�
    DestPort:              Word;        // Ŀ�Ķ˿�
    Length:                Word;        // ���ݰ����ȣ����� UDP ͷ
    Checksum:              Word;        // У���
  end;

  PCnUDPHeader = ^TCnUDPHeader;

{*
  ICMP ��ͷʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |     Type      |     Code      |          Checksum             |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                          �� �� �� ��                          |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnICMPHeader = packed record
    MessageType:           Byte;         // ������Ϣ����
    Code:                  Byte;         // ������Ϣ����
    Checksum:              Word;         // У���
    case Integer of
      0: (Unused:          LongWord);
      1: (Ptr:             Byte;         // ָ��
          Unused1:         Byte;
          Unused2:         Word);
      2: (GatewayAddress:  LongWord);    // ���ص�ַ
      3: (Identifier:      Word;         // ��ʶ
          SequenceNumber:  Word);        // ���к�
  end;

  PCnICMPHeader = ^TCnICMPHeader;

{*
  NTP ��ʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |LI | VN  |Mode |    Stratum    |      Poll     |   Precision   |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                         Root Delay                            |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                         Root Dispersion                       |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                          Reference ID                         |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +        Reference Timestamp (64: 32 Sec, 32 Fraction)          +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +         Origin Timestamp (64: 32 Sec, 32 Fraction)            +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +        Receive Timestamp (64: 32 Sec, 32 Fraction)            +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +        Transmit Timestamp (64: 32 Sec, 32 Fraction)           +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  .                                                               .
  .                    Extension Field 1 (variable)               .
  .                                                               .
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  .                                                               .
  .                    Extension Field 2 (variable)               .
  .                                                               .
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                         Key Identifier                        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  |                          Digest (128)                         |
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

  �ͻ��˽��մ�ʱ��ʱ��� T4 �������˰���
  ʱ���Ϊ 64 λ��ǰ 32 λ�� 1900 �� 1 �� 1 �պ��������
  �� 32 λ��С��������ݣ�ֵΪ΢������ ��2^32/10^6��Ҳ���� 4294.967296 ��
}

  TCnNTPPacket = packed record
    LIVNMode:              Byte;         // �����ʶ���汾�š�����ģʽ
    Stratum:               Byte;         // ϵͳʱ�Ӳ���
    Poll:                  Byte;         // ��ѯ���
    Precision:             Byte;         // ϵͳʱ�Ӿ���
    RootDelay:             LongWord;
    RootDispersion:        LongWord;
    ReferenceID:           LongWord;
    ReferenceTimestamp:    Int64;
    OriginateTimestamp:    Int64;        // �ͻ��˷�������ʱ��ʱ��� T1
    ReceiveTimestamp:      Int64;        // ���������յ������ʱ��� T2
    TransmitTimestamp:     Int64;        // ���������ʹ�ʱ��ʱ��� T3
  end;

  PCnNTPPacket = ^TCnNTPPacket;

{*
  DNS ��ͷ��ʽʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

    0                             1
    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
    7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                      ID                       |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    QDCOUNT                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    ANCOUNT                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    NSCOUNT                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    ARCOUNT                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
}

  TCnDNSHeader = packed record
    Id:                    Word;     // ����ʱ�ͻ������õ� 16 λ��ʶ��������������Ӧ���ʱ������ͬ�ı�ʶ�ֶ�
    QrOpcodeAATCRD:        Byte;     // ����Ӧ�𡢲�ѯ���ࡢ��ȨӦ�𡢽ضϡ������ݹ顢
    RAZRCode:              Byte;     // ֧�ֵݹ顢������Ӧ����
    QDCount:               Word;     // ����������е������¼��
    ANCount:               Word;     // ���Ļش���е������¼��
    NSCount:               Word;     // ������Ȩ���е������¼��
    ARCount:               Word;     // ���ĸ��Ӷ��е������¼��
    SectionData:           array[0..0] of Byte;  // ���ĸ��Ӷ�������ʼ��
  end;

  PCnDNSHeader = ^TCnDNSHeader;

{*
  DNS ��ͷ֮��� Question Section ��ʽ��Ҳ���� QD ��ָʾ�ĸ�ʽ
    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                                               |
  /                     QNAME                     /
  |                                               |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                     QTYPE                     |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                     QCLASS                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

  Name �ǿɱ䳤�����ݣ�������ṹֻ��ʾ��󲿷�
}

  TCnDNSQuestionSectionAfterName = packed record
    QType:                 Word;     // ��ѯ����
    QClass:                Word;     // ��ѯ��
  end;

  PCnDNSQuestionSectionAfterName = ^TCnDNSQuestionSectionAfterName;

{*
  DNS ��ͷ֮��� Resource record ��ʽ��Ҳ���� AN/NS/AR ��ָʾ�ĸ�ʽ
    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                                               |
  /                     NAME                      /
  |                                               |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                     TYPE                      |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    CLASS                      |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                     TTL                       |
  |                                               |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                   RDLENGTH                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
  /                    RDATA                      /
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

  Name �ǿɱ䳤�����ݣ�������ṹֻ��ʾ��󲿷�
}

  TCnDNSResourceRecordAfterName = packed record
    RType:                 Word;     // ��Դ����
    RClass:                Word;     // ��Դ��
    TTL:                   LongWord; // ���ʱ��
    RDLength:              Word;     // ��Դ���ݳ���
    RData:                 array[0..0] of Byte;  // ��Դ���ݣ��� IP ��
  end;

  PCnDNSResourceRecordAfterName = ^TCnDNSResourceRecordAfterName;

{*
  Socks ����Э��ͻ��˷����������ְ�ʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version   |    Method     |    Methods    ...              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksNegotiationRequest = packed record
    Version:               Byte;
    Method:                Byte;
    Methods:               array[1..255] of Byte;
  end;

  PCnSocksNegotiationRequest = ^TCnSocksNegotiationRequest;

{*
  Socks ����Э���������ֻ�Ӧ��ʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1           
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |    Method     |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksNegotiationResponse = packed record
    Version:               Byte;
    Method:                Byte;
  end;

  PCnSocksNegotiationResponse = ^TCnSocksNegotiationResponse;

{*
  Socks ����Э��ͻ����û���������֤�����ʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    | UsernameLen   |    Username  1..255           |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | PasswordLen   |  Password 1..255                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksUsernamePasswordSubNegotiationRequest = packed record
    Version:               Byte;
    UsernameLen:           Byte;
    Username:              array[0..255] of AnsiChar; // 255 ����󳤶ȣ�������ʵ����
    PasswordLen:           Byte;
    Password:              array[1..255] of AnsiChar; // 255 ����󳤶ȣ�������ʵ����
  end;

  PCnSocksUsernamePasswordSubNegotiationRequest = ^TCnSocksUsernamePasswordSubNegotiationRequest;

{*
  Socks ����Э��ͻ����û���������֤��Ӧ��ʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1           
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |    Status     |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksUsernamePasswordSubNegotiationResponse = packed record
    Version:               Byte;
    Status:                Byte;
  end;

  PCnSocksUsernamePasswordSubNegotiationResponse = ^TCnSocksUsernamePasswordSubNegotiationResponse;

{*
  Socks ����Э��ͻ��������ʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |    Command    |   Reserved    |  Adress Type  |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Destination Address        |        Destination Port       |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksAddress = packed record
    case Integer of
      0: (IpV4Address:     Cardinal);
      1: (DomainNameLen:   Byte;
         DomainName:       array[0..255] of AnsiChar);
      2: (IpV6Address:     array[0..15] of AnsiChar);
  end;

  TCnSocksRequest = packed record
    Version:               Byte;
    Command:               Byte;
    Reserved:              Byte;
    AddressType:           Byte;
    DestionationAddress:   TCnSocksAddress;
    DestionationPort:      array[0..1] of AnsiChar;   // �����ֶοɱ䳤�����ֶ�λ�ò��̶�
  end;

  PCnSocksRequest = ^TCnSocksRequest;

{*
  Socks ����Э������Ӧ���ʾ��ͼ���ֽ�������Ǹ�λ���ұ��ǵ�λ��
  �ֽ�֮����� Big-Endian �������ֽ�˳�򣬸�λ�ڵ͵�ַ�������Ķ�ϰ�ߡ�

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |     Reply     |   Reserved    |  Adress Type  |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |         Bind Address          |            Bind Port          |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksResponse = packed record
    Version:               Byte;
    Reply:                 Byte;
    Reserved:              Byte;
    AddressType:           Byte;
    BindAddress:           TCnSocksAddress;
    BindPort:              array[0..1] of AnsiChar;   // ��һ�ֶοɱ䳤�����ֶ�λ�ò��̶�
  end;

  PCnSocksResponse = ^TCnSocksResponse;

// ======================== IP ��ͷϵ�к��� ====================================

function CnGetIPVersion(const IPHeader: PCnIPHeader): Integer;
{* ��� IP ��ͷ�ڵ� IP �汾��}

function CnGetIPHeaderLength(const IPHeader: PCnIPHeader): Integer;
{* ��� IP ��ͷ�ڵ� IP ��ͷ���ȣ���λΪ 4 �ֽ�}

function CnGetIPTypeOfServicePrecedence(const IPHeader: PCnIPHeader): Integer;
{* ��� IP ��ͷ�ڵ� Type of Service �ֶ��е� Precedence ֵ}

function CnGetIPTypeOfServiceDelay(const IPHeader: PCnIPHeader): Boolean;
{* ��� IP ��ͷ�ڵ� Type of Service �ֶ��е� Delay ֵ��True Ϊ Low��False Ϊ Normal}

function CnGetIPTypeOfServiceThroughput(const IPHeader: PCnIPHeader): Boolean;
{* ��� IP ��ͷ�ڵ� Type of Service �ֶ��е� Throughput ֵ��True Ϊ High��False Ϊ Normal}

function CnGetIPTypeOfServiceRelibility(const IPHeader: PCnIPHeader): Boolean;
{* ��� IP ��ͷ�ڵ� Type of Service �ֶ��е� Relibility ֵ��True Ϊ High��False Ϊ Normal}

function CnGetIPTotalLength(const IPHeader: PCnIPHeader): Integer;
{* ��� IP ��ͷ�ڵİ��ܳ��ȣ����������ֽ�ת��}

function CnGetIPIdentification(const IPHeader: PCnIPHeader): Integer;
{* ��� IP ��ͷ�ڵı�ʶ�����������ֽ�ת��}

function CnGetIPFlagDontFragment(const IPHeader: PCnIPHeader): Boolean;
{* ��� IP ��ͷ�ڵ��Ƿ�ֶα�ǣ����� True Ϊ���ֶΣ�False Ϊ����ֶ�}

function CnGetIPFlagMoreFragment(const IPHeader: PCnIPHeader): Boolean;
{* ��� IP ��ͷ�ڵ��Ƿ��и���ֶα�ǣ����� True Ϊ�и���ֶΣ�False Ϊ���һ���ֶ�}

function CnGetIPFragmentOffset(const IPHeader: PCnIPHeader): Integer;
{* ��� IP ��ͷ�ڵķֶ�ƫ�ƣ����������ֽ�ת��}

function CnGetIPChecksum(const IPHeader: PCnIPHeader): Word;
{* ��� IP ��ͷ�ڵ�У��ͣ����������ֽ�ת��}

function CnGetIPSourceIP(const IPHeader: PCnIPHeader): LongWord;
{* ��� IP ��ͷ�ڵ�Դ IP ��ַ�����������ֽ�ת��}

function CnGetIPDestIP(const IPHeader: PCnIPHeader): LongWord;
{* ��� IP ��ͷ�ڵ�Ŀ�� IP ��ַ�����������ֽ�ת��}

// ======================== TCP ��ͷϵ�к��� ===================================

function CnGetTCPSourcePort(const TCPHeader: PCnTCPHeader): Integer;
{* ��� TCP ��ͷ�ڵ�Դ�˿ںţ����������ֽ�ת��}

function CnGetTCPDestPort(const TCPHeader: PCnTCPHeader): Integer;
{* ��� TCP ��ͷ�ڵ�Ŀ�Ķ˿ںţ����������ֽ�ת��}

function CnGetTCPSequenceNumber(const TCPHeader: PCnTCPHeader): LongWord;
{* ��� TCP ��ͷ�ڵ����кţ����������ֽ�ת��}

function CnGetTCPAcknowledgementNumber(const TCPHeader: PCnTCPHeader): LongWord;
{* ��� TCP ��ͷ�ڵ���Ӧ�ţ����������ֽ�ת��}

function CnGetTCPOffset(const TCPHeader: PCnTCPHeader): Integer;
{* ��� TCP ��ͷ�ڵ�����ƫ��ֵ}

function CnGetTCPFlagURG(const TCPHeader: PCnTCPHeader): Boolean;
{* ��� TCP ��ͷ���Ƿ��� URG ��ǣ����򷵻� True�����򷵻� False}

function CnGetTCPFlagACK(const TCPHeader: PCnTCPHeader): Boolean;
{* ��� TCP ��ͷ���Ƿ��� ACK ��ǣ����򷵻� True�����򷵻� False}

function CnGetTCPFlagPSH(const TCPHeader: PCnTCPHeader): Boolean;
{* ��� TCP ��ͷ���Ƿ��� PSH ��ǣ����򷵻� True�����򷵻� False}

function CnGetTCPFlagRST(const TCPHeader: PCnTCPHeader): Boolean;
{* ��� TCP ��ͷ���Ƿ��� RST ��ǣ����򷵻� True�����򷵻� False}

function CnGetTCPFlagSYN(const TCPHeader: PCnTCPHeader): Boolean;
{* ��� TCP ��ͷ���Ƿ��� SYN ��ǣ����򷵻� True�����򷵻� False}

function CnGetTCPFlagFIN(const TCPHeader: PCnTCPHeader): Boolean;
{* ��� TCP ��ͷ���Ƿ��� FIN ��ǣ����򷵻� True�����򷵻� False}

function CnGetTCPWindow(const TCPHeader: PCnTCPHeader): Integer;
{* ��� TCP ��ͷ�ڵĴ��ڴ�С�����������ֽ�ת��}

function CnGetTCPChecksum(const TCPHeader: PCnTCPHeader): Word;
{* ��� TCP ��ͷ�ڵ�У��ͣ����������ֽ�ת��}

function CnGetTCPUrgentPointer(const TCPHeader: PCnTCPHeader): Word;
{* ��� TCP ��ͷ�ڵĽ���ָ�룬���������ֽ�ת��}

// ======================== UDP ��ͷϵ�к��� ===================================

function CnGetUDPSourcePort(const UDPHeader: PCnUDPHeader): Integer;
{* ��� UDP ��ͷ�ڵ�Դ�˿ںţ����������ֽ�ת��}

function CnGetUDPDestPort(const UDPHeader: PCnUDPHeader): Integer;
{* ��� UDP ��ͷ�ڵ�Ŀ�Ķ˿ںţ����������ֽ�ת��}

function CnGetUDPLength(const UDPHeader: PCnUDPHeader): Integer;
{* ��� UDP ��ͷ�ڵİ��ܳ��ȣ����������ֽ�ת��}

function CnGetUDPChecksum(const UDPHeader: PCnUDPHeader): Word;
{* ��� UDP ��ͷ�ڵ�У��ͣ����������ֽ�ת��}

// ======================== ICMP ��ͷϵ�к��� ==================================

function CnGetICMPType(const ICMPHeader: PCnICMPHeader): Integer;
{* ��� ICMP ��ͷ�ڵ���Ϣ����}

function CnGetICMPCode(const ICMPHeader: PCnICMPHeader): Integer;
{* ��� ICMP ��ͷ�ڵ���Ϣ����}

function CnGetICMPChecksum(const ICMPHeader: PCnICMPHeader): Word;
{* ��� ICMP ��ͷ�ڵ�У���}

function CnGetICMPPointer(const ICMPHeader: PCnICMPHeader): Integer;
{* ��� ICMP ��ͷ�ڵ�ָ���ֶ�ֵ}

function CnGetICMPGatewayAddress(const ICMPHeader: PCnICMPHeader): LongWord;
{* ��� ICMP ��ͷ�ڵ����ص�ַ}

function CnGetICMPIdentifier(const ICMPHeader: PCnICMPHeader): Word;
{* ��� ICMP ��ͷ�ڵı�ʶ��}

function CnGetICMPSequenceNumber(const ICMPHeader: PCnICMPHeader): Word;
{* ��� ICMP ��ͷ�ڵ����к�}

// ========================== NTP ��ϵ�к��� ===================================

function CnGetNTPLeapIndicator(const NTPPacket: PCnNTPPacket): Integer;
{* ��� NTP ���ڵ������ʶ}

function CnGetNTPVersionNumber(const NTPPacket: PCnNTPPacket): Integer;
{* ��� NTP ���ڵİ汾��}

function CnGetNTPMode(const NTPPacket: PCnNTPPacket): Integer;
{* ��� NTP ���ڵ�ģʽ}

procedure CnSetNTPLeapIndicator(const NTPPacket: PCnNTPPacket; LeapIndicator: Integer);
{* ���� NTP ���ڵ������ʶ��ʹ�� CN_NTP_LEAP_INDICATOR_* ϵ�г��� }

procedure CnSetNTPVersionNumber(const NTPPacket: PCnNTPPacket; VersionNumber: Integer);
{* ���� NTP ���ڵİ汾�ţ�ʹ�� CN_NTP_VERSION_* ϵ�г���}

procedure CnSetNTPMode(const NTPPacket: PCnNTPPacket; NTPMode: Integer);
{* ���� NTP ���ڵ�ģʽ��ʹ�� CN_NTP_MODE_* ϵ�г���}

function CnConvertNTPTimestampToDateTime(Stamp: Int64): TDateTime;
{* �� NTP ���е�ʱ���ֵת��������ʱ��}

function CnConvertDateTimeToNTPTimestamp(ADateTime: TDateTime): Int64;
{* ������ʱ��ת���� NTP ���е�ʱ���ֵ}

// ========================== DNS ��ϵ�к��� ===================================

function CnGetDNSHeaderId(const DNSHeader: PCnDNSHeader): Word;
{* ��� DNS ��ͷ�ڵ� Id}

function CnGetDNSHeaderQR(const DNSHeader: PCnDNSHeader): Integer;
{* ��� DNS ��ͷ�ڵ� QR ��ʶ����ѯ����Ӧ������ CN_DNS_HEADER_TYPE_*}

function CnGetDNSHeaderOpCode(const DNSHeader: PCnDNSHeader): Integer;
{* ��� DNS ��ͷ�ڵ� OpCode ��ѯ���࣬���� CN_DNS_HEADER_OPCODE_*}

function CnGetDNSHeaderAA(const DNSHeader: PCnDNSHeader): Boolean;
{* ��� DNS ��ͷ�ڵ���ȨӦ��λ�Ƿ���λ������ True �� False}

function CnGetDNSHeaderTC(const DNSHeader: PCnDNSHeader): Boolean;
{* ��� DNS ��ͷ�ڵĽض�λ�Ƿ���λ������ True �� False}

function CnGetDNSHeaderRD(const DNSHeader: PCnDNSHeader): Boolean;
{* ��� DNS ��ͷ�ڵ������ݹ�λ�Ƿ���λ������ True �� False}

function CnGetDNSHeaderRA(const DNSHeader: PCnDNSHeader): Boolean;
{* ��� DNS ��ͷ�ڵ�֧�ֵݹ�λ�Ƿ���λ������ True �� False}

function CnGetDNSHeaderRCode(const DNSHeader: PCnDNSHeader): Integer;
{* ��� DNS ��ͷ�ڵ�Ӧ���룬���� CN_DNS_HEADER_RCODE_*}

function CnGetDNSHeaderQDCount(const DNSHeader: PCnDNSHeader): Integer;
{* ��� DNS ��ͷ�ڵ������¼����}

function CnGetDNSHeaderANCount(const DNSHeader: PCnDNSHeader): Integer;
{* ��� DNS ��ͷ�ڵĻش��¼����}

function CnGetDNSHeaderNSCount(const DNSHeader: PCnDNSHeader): Integer;
{* ��� DNS ��ͷ�ڵ���Ȩ��¼����}

function CnGetDNSHeaderARCount(const DNSHeader: PCnDNSHeader): Integer;
{* ��� DNS ��ͷ�ڵĸ��Ӽ�¼����}

procedure CnSetDNSHeaderId(const DNSHeader: PCnDNSHeader; Id: Word);
{* ���� DNS ��ͷ�ڵ� Id}

procedure CnSetDNSHeaderQR(const DNSHeader: PCnDNSHeader; IsQuery: Boolean);
{* ���� DNS ��ͷ�ڵ� QR ��ʶ���ǲ�ѯ������Ӧ}

procedure CnSetDNSHeaderOpCode(const DNSHeader: PCnDNSHeader; QueryType: Byte);
{* ���� DNS ��ͷ�ڵ� OpCode ��ѯ���࣬ʹ�� CN_DNS_HEADER_OPCODE_*}

procedure CnSetDNSHeaderAA(const DNSHeader: PCnDNSHeader; AuthoritativeAnswer: Boolean);
{* ���� DNS ��ͷ�ڵ���ȨӦ��λ�Ƿ���λ}

procedure CnSetDNSHeaderTC(const DNSHeader: PCnDNSHeader; TrunCation: Boolean);
{* ���� DNS ��ͷ�ڵĽض�λ�Ƿ���λ}

procedure CnSetDNSHeaderRD(const DNSHeader: PCnDNSHeader; RecursionDesired: Boolean);
{* ���� DNS ��ͷ�ڵ������ݹ�λ�Ƿ���λ}

procedure CnSetDNSHeaderRA(const DNSHeader: PCnDNSHeader; RecursionAvailable: Boolean);
{* ���� DNS ��ͷ�ڵ�֧�ֵݹ�λ�Ƿ���λ}

procedure CnSetDNSHeaderRCode(const DNSHeader: PCnDNSHeader; RCode: Byte);
{* ���� DNS ��ͷ�ڵ�Ӧ���룬ʹ�� CN_DNS_HEADER_RCODE_*}

procedure CnSetDNSHeaderQDCount(const DNSHeader: PCnDNSHeader; Count: Word);
{* ���� DNS ��ͷ�ڵ������¼����}

procedure CnSetDNSHeaderANCount(const DNSHeader: PCnDNSHeader; Count: Word);
{* ���� DNS ��ͷ�ڵĻش��¼����}

procedure CnSetDNSHeaderNSCount(const DNSHeader: PCnDNSHeader; Count: Word);
{* ���� DNS ��ͷ�ڵ���Ȩ��¼����}

procedure CnSetDNSHeaderARCount(const DNSHeader: PCnDNSHeader; Count: Word);
{* ���� DNS ��ͷ�ڵĸ��Ӽ�¼����}

// ======================== Socks �����ϵ�к��� ===============================

function CnGetSocksRequestDestinationAddress(const SocksReq: PCnSocksRequest): string;
{* ���� Socks �����е�Ŀ�ĵ�ַ��֧�� IPv4/v6 ������������}

function CnGetSocksRequestDestinationPort(const SocksReq: PCnSocksRequest): Word;
{* ���� Socks �����е�Ŀ�ĵ�ַ�˿�}

procedure CnSetSocksRequestDestinationAddress(const SocksReq: PCnSocksRequest; Address: string);
{* ���� Socks �����е�Ŀ�ĵ�ַ��֧�� IPv4 ������������}

function CnSetSocksRequestDestinationPort(const SocksReq: PCnSocksRequest; Port: Word): Integer;
{* ���� Socks �����е�Ŀ�Ķ˿ںţ����� SocksReq �ṹ�ܳ���}

function CnGetSocksResponseBindAddress(const SocksResp: PCnSocksResponse): string;
{* ���� Socks Ӧ���еİ󶨵�ַ��֧�� IPv4/v6 ������������}

function CnGetSocksResponseBindPort(const SocksResp: PCnSocksResponse): Word;
{* ���� Socks Ӧ���еİ󶨶˿ں�}

procedure CnSetSocksResponseBindAddress(const SocksResp: PCnSocksResponse; Address: string);
{* ���� Socks Ӧ���еİ󶨵�ַ��֧�� IPv4/v6 ������������}

function CnSetSocksResponseBindPort(const SocksResp: PCnSocksResponse; Port: Word): Integer;
{* ���� Socks Ӧ���еİ󶨶˿ںţ����� SocksResp �ṹ�ܳ���}

// ========================= �ֽ�˳��������� ==================================

function CnNetworkToHostWord(Value: Word): Word;

function CnHostToNetworkWord(Value: Word): Word;

function CnNetworkToHostLongWord(Value: LongWord): LongWord;

function CnHostToNetworkLongWord(Value: LongWord): LongWord;

function CnNetworkToHostInt64(Value: Int64): Int64;

function CnHostToNetworkInt64(Value: Int64): Int64;

// =========================== IP ��ַת������ =================================

function CardinalToIPString(const IP: Cardinal): string;
{* IPv4 ����ת��Ϊ�ַ�����IP Ҫ��Ϊ Host �ֽ�˳�򣬴�������ȡ��ʱ��Ҫת��}

function IPStringToCardinal(const IP: string): Cardinal;
{* IPv4 �ַ���ת��Ϊ���ͣ����Ϊ Host �ֽ�˳�����紫��ʱ��Ҫ��ת��}

implementation

function CnNetworkToHostWord(Value: Word): Word;
begin
  Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8);
end;

function CnHostToNetworkWord(Value: Word): Word;
begin
  Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8);
end;

function CnNetworkToHostLongWord(Value: LongWord): LongWord;
begin
  Result := ((Value and $000000FF) shl 24) or ((Value and $0000FF00) shl 8)
    or ((Value and $00FF0000) shr 8) or ((Value and $FF000000) shr 24);
end;

function CnHostToNetworkLongWord(Value: LongWord): LongWord;
begin
  Result := ((Value and $000000FF) shl 24) or ((Value and $0000FF00) shl 8)
    or ((Value and $00FF0000) shr 8) or ((Value and $FF000000) shr 24);
end;

function CnNetworkToHostInt64(Value: Int64): Int64;
var
  Lo, Hi: LongWord;
  Rec: Int64Rec;
begin
  Lo := Int64Rec(Value).Lo;
  Hi := Int64Rec(Value).Hi;
  Lo := CnNetworkToHostLongWord(Lo);
  Hi := CnNetworkToHostLongWord(Hi);
  Rec.Lo := Hi;
  Rec.Hi := Lo;
  Result := Int64(Rec);
end;

function CnHostToNetworkInt64(Value: Int64): Int64;
var
  Lo, Hi: LongWord;
  Rec: Int64Rec;
begin
  Lo := Int64Rec(Value).Lo;
  Hi := Int64Rec(Value).Hi;
  Lo := CnNetworkToHostLongWord(Lo);
  Hi := CnNetworkToHostLongWord(Hi);
  Rec.Lo := Hi;
  Rec.Hi := Lo;
  Result := Int64(Rec);
end;

function CardinalToIPString(const IP: Cardinal): string;
var
  A, B, C, D: Byte;
begin
  A := IP and $FF000000 shr 24;
  B := IP and $00FF0000 shr 16;
  C := IP and $0000FF00 shr 8;
  D := IP and $000000FF;
  Result := Format('%d.%d.%d.%d', [A, B, C, D]);
end;

function IPStringToCardinal(const IP: string): Cardinal;
var
  MyIP: string;
  P: Integer;
  A, B, C, D: string;
  AA, BB, CC, DD: Byte;
begin
  Result := 0;
  MyIP := IP;

  P := Pos('.', MyIP);
  if P = 0 then
    Exit;

  A := Copy(MyIP, 1, P - 1);
  Delete(MyIP, 1, P);

  P := Pos('.', MyIP);
  if P = 0 then
    Exit;

  B := Copy(MyIP, 1, P - 1);
  Delete(MyIP, 1, P);

  P := Pos('.', MyIP);
  if P = 0 then
    Exit;

  C := Copy(MyIP, 1, P - 1);
  Delete(MyIP, 1, P);

  D := Copy(MyIP, 1, MaxInt);

  try
    AA := StrToInt(A);
    BB := StrToInt(B);
    CC := StrToInt(C);
    DD := StrToInt(D);
  except
    Exit;
  end;
  Result := (AA shl 24) or (BB shl 16) or (CC shl 8) or DD;
end;

function CnGetIPVersion(const IPHeader: PCnIPHeader): Integer;
begin
  Result := (IPHeader^.VerionHeaderLength and $F0) shr 4;
end;

function CnGetIPHeaderLength(const IPHeader: PCnIPHeader): Integer;
begin
  Result := IPHeader^.VerionHeaderLength and $0F;
end;

function CnGetIPTypeOfServicePrecedence(const IPHeader: PCnIPHeader): Integer;
begin
  Result := IPHeader^.TypeOfService shr 5;
end;

function CnGetIPTypeOfServiceDelay(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (IPHeader^.TypeOfService and CN_IP_TOS_DELAY_MASK) <> 0;
end;

function CnGetIPTypeOfServiceThroughput(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (IPHeader^.TypeOfService and CN_IP_TOS_THROUGHPUT_MASK) <> 0;
end;

function CnGetIPTypeOfServiceRelibility(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (IPHeader^.TypeOfService and CN_IP_TOS_RELIBILITY_MASK) <> 0;
end;

function CnGetIPTotalLength(const IPHeader: PCnIPHeader): Integer;
begin
  Result := CnNetworkToHostWord(IPHeader^.TotalLength);
end;

function CnGetIPIdentification(const IPHeader: PCnIPHeader): Integer;
begin
  Result := CnNetworkToHostWord(IPHeader^.Identification);
end;

function CnGetIPFlagDontFragment(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (CnNetworkToHostWord(IPHeader^.FlagOffset) and CN_IP_FLAG_DONT_FRAGMENT_WORD_MASK) <> 0;
end;

function CnGetIPFlagMoreFragment(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (CnNetworkToHostWord(IPHeader^.FlagOffset) and CN_IP_FLAG_MORE_FRAGMENT_WORD_MASK) <> 0;
end;

function CnGetIPFragmentOffset(const IPHeader: PCnIPHeader): Integer;
begin
  Result := CnNetworkToHostWord(IPHeader^.FlagOffset) and CN_IP_FLAG_FRAGMENT_OFFSET_WORD_MASK;
end;

function CnGetIPChecksum(const IPHeader: PCnIPHeader): Word;
begin
  Result := CnNetworkToHostWord(IPHeader^.Checksum);
end;

function CnGetIPSourceIP(const IPHeader: PCnIPHeader): LongWord;
begin
  Result := CnNetworkToHostLongWord(IPHeader^.SourceIp);
end;

function CnGetIPDestIP(const IPHeader: PCnIPHeader): LongWord;
begin
  Result := CnNetworkToHostLongWord(IPHeader^.DestIp);
end;

function CnGetTCPSourcePort(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := CnNetworkToHostWord(TCPHeader^.SourcePort);
end;

function CnGetTCPDestPort(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := CnNetworkToHostWord(TCPHeader^.DestPort);
end;

function CnGetTCPSequenceNumber(const TCPHeader: PCnTCPHeader): LongWord;
begin
  Result := CnNetworkToHostLongWord(TCPHeader^.SequenceNumber);
end;

function CnGetTCPAcknowledgementNumber(const TCPHeader: PCnTCPHeader): LongWord;
begin
  Result := CnNetworkToHostLongWord(TCPHeader^.AcknowledgementNumber);
end;

function CnGetTCPOffset(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := TCPHeader^.Offset shr 4;
end;

function CnGetTCPFlagURG(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_URG_MASK) <> 0;
end;

function CnGetTCPFlagACK(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_ACK_MASK) <> 0;
end;

function CnGetTCPFlagPSH(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_PSH_MASK) <> 0;
end;

function CnGetTCPFlagRST(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_RST_MASK) <> 0;
end;

function CnGetTCPFlagSYN(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_SYN_MASK) <> 0;
end;

function CnGetTCPFlagFIN(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_FIN_MASK) <> 0;
end;

function CnGetTCPWindow(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := CnNetworkToHostWord(TCPHeader^.Window);
end;

function CnGetTCPChecksum(const TCPHeader: PCnTCPHeader): Word;
begin
  Result := CnNetworkToHostWord(TCPHeader^.Checksum);
end;

function CnGetTCPUrgentPointer(const TCPHeader: PCnTCPHeader): Word;
begin
  Result := CnNetworkToHostWord(TCPHeader^.UrgentPointer);
end;

function CnGetUDPSourcePort(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := CnNetworkToHostWord(UDPHeader^.SourcePort);
end;

function CnGetUDPDestPort(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := CnNetworkToHostWord(UDPHeader^.DestPort);
end;

function CnGetUDPLength(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := CnNetworkToHostWord(UDPHeader^.Length);
end;

function CnGetUDPChecksum(const UDPHeader: PCnUDPHeader): Word;
begin
  Result := CnNetworkToHostWord(UDPHeader^.Checksum);
end;

function CnGetICMPType(const ICMPHeader: PCnICMPHeader): Integer;
begin
  Result := ICMPHeader^.MessageType;
end;

function CnGetICMPCode(const ICMPHeader: PCnICMPHeader): Integer;
begin
  Result := ICMPHeader^.Code;
end;

function CnGetICMPChecksum(const ICMPHeader: PCnICMPHeader): Word;
begin
  Result := CnNetworkToHostWord(ICMPHeader^.Checksum);
end;

function CnGetICMPPointer(const ICMPHeader: PCnICMPHeader): Integer;
begin
  Result := ICMPHeader^.Ptr;
end;

function CnGetICMPGatewayAddress(const ICMPHeader: PCnICMPHeader): LongWord;
begin
  Result := CnNetworkToHostLongWord(ICMPHeader^.GatewayAddress);
end;

function CnGetICMPIdentifier(const ICMPHeader: PCnICMPHeader): Word;
begin
  Result := CnNetworkToHostWord(ICMPHeader^.Identifier);
end;

function CnGetICMPSequenceNumber(const ICMPHeader: PCnICMPHeader): Word;
begin
  Result := CnNetworkToHostWord(ICMPHeader^.SequenceNumber);
end;

function CnGetNTPLeapIndicator(const NTPPacket: PCnNTPPacket): Integer;
begin
  Result := (NTPPacket^.LIVNMode and $C0) shr 6;
end;

function CnGetNTPVersionNumber(const NTPPacket: PCnNTPPacket): Integer;
begin
  Result := (NTPPacket^.LIVNMode and $38) shr 3;
end;

function CnGetNTPMode(const NTPPacket: PCnNTPPacket): Integer;
begin
  Result := NTPPacket^.LIVNMode and $07;
end;

procedure CnSetNTPLeapIndicator(const NTPPacket: PCnNTPPacket; LeapIndicator: Integer);
begin
  NTPPacket^.LIVNMode := NTPPacket^.LIVNMode or ((LeapIndicator and $03) shl 6);
end;

procedure CnSetNTPVersionNumber(const NTPPacket: PCnNTPPacket; VersionNumber: Integer);
begin
  NTPPacket^.LIVNMode := NTPPacket^.LIVNMode or ((VersionNumber and $07) shl 3);
end;

procedure CnSetNTPMode(const NTPPacket: PCnNTPPacket; NTPMode: Integer);
begin
  NTPPacket^.LIVNMode := NTPPacket^.LIVNMode or (NTPMode and $07);
end;

function CnConvertNTPTimestampToDateTime(Stamp: Int64): TDateTime;
var
  Sec, Frac: DWORD;
begin
  Stamp := CnNetworkToHostInt64(Stamp);
  Sec := Int64Rec(Stamp).Hi;
  Frac := Int64Rec(Stamp).Lo;

  // Sec �������� 1900��1��1��0�㿪ʼ���� TDateTime ��������1899��12��30��0�㿪ʼ��������
  Result := 2 + (Sec div 86400) + (Sec mod 86400) / 86400.00 +
    Frac / (CN_NTP_MICRO_SEC_FRACTION * 1000 * 1000 * 86400.00);
end;

function CnConvertDateTimeToNTPTimestamp(ADateTime: TDateTime): Int64;
var
  H, M, S, Ms: Word;
  Sec, Frac: DWORD;
begin
  // Sec �������� 1900��1��1��0�㿪ʼ���� TDateTime ��������1899��12��30��0�㿪ʼ��������
  ADateTime := ADateTime - 2;
  DecodeTime(ADateTime, H, M, S, Ms);
  Sec := Trunc(ADateTime) * 86400 + H * 3600 + M * 60 + S;
  Frac := Trunc(Ms * 1000 * CN_NTP_MICRO_SEC_FRACTION);

  Int64Rec(Result).Lo := Frac;
  Int64Rec(Result).Hi := Sec;
  Result := CnHostToNetworkInt64(Result); // ����ת��
end;

function CnGetDNSHeaderId(const DNSHeader: PCnDNSHeader): Word;
begin
  Result := CnNetworkToHostWord(DNSHeader^.Id);
end;

function CnGetDNSHeaderQR(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $80) shr 7;
end;

function CnGetDNSHeaderOpCode(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $78) shr 3;
end;

function CnGetDNSHeaderAA(const DNSHeader: PCnDNSHeader): Boolean;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $04) <> 0;
end;

function CnGetDNSHeaderTC(const DNSHeader: PCnDNSHeader): Boolean;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $02) <> 0;
end;

function CnGetDNSHeaderRD(const DNSHeader: PCnDNSHeader): Boolean;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $01) <> 0;
end;

function CnGetDNSHeaderRA(const DNSHeader: PCnDNSHeader): Boolean;
begin
  Result := (DNSHeader^.RAZRCode and $80) <> 0;
end;

function CnGetDNSHeaderRCode(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := DNSHeader^.RAZRCode and $0F;
end;

function CnGetDNSHeaderQDCount(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := CnNetworkToHostWord(DNSHeader^.QDCount);
end;

function CnGetDNSHeaderANCount(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := CnNetworkToHostWord(DNSHeader^.ANCount);
end;

function CnGetDNSHeaderNSCount(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := CnNetworkToHostWord(DNSHeader^.NSCount);
end;

function CnGetDNSHeaderARCount(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := CnNetworkToHostWord(DNSHeader^.ARCount);
end;

procedure CnSetDNSHeaderId(const DNSHeader: PCnDNSHeader; Id: Word);
begin
  DNSHeader^.Id := CnHostToNetworkWord(Id);
end;

procedure CnSetDNSHeaderQR(const DNSHeader: PCnDNSHeader; IsQuery: Boolean);
begin
  if IsQuery then
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD and $7F
  else
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD or $80;
end;

procedure CnSetDNSHeaderOpCode(const DNSHeader: PCnDNSHeader; QueryType: Byte);
begin
  DNSHeader^.QrOpcodeAATCRD := (DNSHeader^.QrOpcodeAATCRD and $87) or Byte(QueryType shl 3);
end;

procedure CnSetDNSHeaderAA(const DNSHeader: PCnDNSHeader; AuthoritativeAnswer: Boolean);
begin
  if AuthoritativeAnswer then
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD or $04
  else
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD and $FB;
end;

procedure CnSetDNSHeaderTC(const DNSHeader: PCnDNSHeader; TrunCation: Boolean);
begin
  if TrunCation then
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD or $02
  else
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD and $FD;
end;

procedure CnSetDNSHeaderRD(const DNSHeader: PCnDNSHeader; RecursionDesired: Boolean);
begin
  if RecursionDesired then
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD or $01
  else
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD and $FE;
end;

procedure CnSetDNSHeaderRA(const DNSHeader: PCnDNSHeader; RecursionAvailable: Boolean);
begin
  if RecursionAvailable then
    DNSHeader^.RAZRCode := DNSHeader^.RAZRCode or $80
  else
    DNSHeader^.RAZRCode := DNSHeader^.RAZRCode and $7F;
end;

procedure CnSetDNSHeaderRCode(const DNSHeader: PCnDNSHeader; RCode: Byte);
begin
  DNSHeader^.RAZRCode := (DNSHeader^.RAZRCode and $F0) or (RCode and $0F);
end;

procedure CnSetDNSHeaderQDCount(const DNSHeader: PCnDNSHeader; Count: Word);
begin
  DNSHeader^.QDCount := CnHostToNetworkWord(Count);
end;

procedure CnSetDNSHeaderANCount(const DNSHeader: PCnDNSHeader; Count: Word);
begin
  DNSHeader^.ANCount := CnHostToNetworkWord(Count);
end;

procedure CnSetDNSHeaderNSCount(const DNSHeader: PCnDNSHeader; Count: Word);
begin
  DNSHeader^.NSCount := CnHostToNetworkWord(Count);
end;

procedure CnSetDNSHeaderARCount(const DNSHeader: PCnDNSHeader; Count: Word);
begin
  DNSHeader^.ARCount := CnHostToNetworkWord(Count);
end;

function CnGetSocksRequestDestinationAddress(const SocksReq: PCnSocksRequest): string;
var
  Len: Integer;
  Res: AnsiString;
begin
  Result := '';
  case SocksReq^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      begin
        Result := CardinalToIPString(CnNetworkToHostLongWord(SocksReq^.DestionationAddress.IpV4Address));
      end;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      begin
        // TODO: IPv6
        raise Exception.Create('NOT Implemented.');
      end;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksReq^.DestionationAddress.DomainNameLen;
        SetLength(Res, Len);
        Move(SocksReq^.DestionationAddress.DomainName, Res[1], Len);
        Result := string(Res);
      end;
  end;
end;

function CnGetSocksRequestDestinationPort(const SocksReq: PCnSocksRequest): Word;
var
  Len: Integer;
  PortAddr: PWORD;
begin
  Result := 0;
  Len := 0;

  case SocksReq^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      Len := 4;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      Len := 6;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksReq^.DestionationAddress.DomainNameLen + 1;
      end;
  end;

  if Len > 0 then
  begin
    PortAddr := PWORD(Integer(@(SocksReq^.DestionationAddress)) + Len);
    Result := CnNetworkToHostWord(PortAddr^);
  end;
end;

function CnGetSocksResponseBindAddress(const SocksResp: PCnSocksResponse): string;
var
  Len: Integer;
  Res: AnsiString;
begin
  Result := '';
  case SocksResp^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      begin
        Result := CardinalToIPString(CnNetworkToHostLongWord(SocksResp^.BindAddress.IpV4Address));
      end;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      begin
        // TODO: IPv6
        raise Exception.Create('NOT Implemented.');
      end;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksResp^.BindAddress.DomainNameLen;
        SetLength(Res, Len);
        Move(SocksResp^.BindAddress.DomainName, Res[1], Len);
        Result := string(Res);
      end;
  end;
end;

function CnGetSocksResponseBindPort(const SocksResp: PCnSocksResponse): Word;
var
  Len: Integer;
  PortAddr: PWORD;
begin
  Result := 0;
  Len := 0;

  case SocksResp^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      Len := 4;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      Len := 6;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksResp^.BindAddress.DomainNameLen + 1;
      end;
  end;

  if Len > 0 then
  begin
    PortAddr := PWORD(Integer(@(SocksResp^.BindAddress)) + Len);
    Result := CnNetworkToHostWord(PortAddr^);
  end;
end;

procedure CnSetSocksRequestDestinationAddress(const SocksReq: PCnSocksRequest; Address: string);
var
  IP: Cardinal;
  AnsiAddress: AnsiString;
begin
  IP := CnHostToNetworkLongWord(IPStringToCardinal(Address));
  if IP = 0 then // �Ƿ� IP����ʾ������
  begin
    SocksReq^.AddressType := CN_SOCKS_ADDRESS_TYPE_DOMAINNAME;
    AnsiAddress := AnsiString(Address);
    SocksReq^.DestionationAddress.DomainNameLen := Length(AnsiAddress);
    if AnsiAddress <> '' then
      Move(AnsiAddress[1], SocksReq^.DestionationAddress.DomainName[0],
        SocksReq^.DestionationAddress.DomainNameLen);
  end
  else
  begin
    SocksReq^.AddressType := CN_SOCKS_ADDRESS_TYPE_IPV4;
    SocksReq^.DestionationAddress.IpV4Address := IP;
  end;
end;

function CnSetSocksRequestDestinationPort(const SocksReq: PCnSocksRequest;
  Port: Word): Integer;
var
  Len: Integer;
  PortAddr: PWORD;
begin
  Len := 0;

  case SocksReq^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      Len := 4;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      Len := 6;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksReq^.DestionationAddress.DomainNameLen + 1;
      end;
  end;

  if Len > 0 then
  begin
    PortAddr := PWORD(Integer(@(SocksReq^.DestionationAddress)) + Len);
    PortAddr^ := CnHostToNetworkWord(Port);
  end;
  Result := Len + 4 + SizeOf(Word);
end;

procedure CnSetSocksResponseBindAddress(const SocksResp: PCnSocksResponse; Address: string);
var
  IP: Cardinal;
  AnsiAddress: AnsiString;
begin
  IP := CnHostToNetworkLongWord(IPStringToCardinal(Address));
  if IP = 0 then // �Ƿ� IP����ʾ������
  begin
    SocksResp^.AddressType := CN_SOCKS_ADDRESS_TYPE_DOMAINNAME;
    AnsiAddress := AnsiString(Address);
    SocksResp^.BindAddress.DomainNameLen := Length(AnsiAddress);
    if AnsiAddress <> '' then
      Move(AnsiAddress[1], SocksResp^.BindAddress.DomainName[0],
        SocksResp^.BindAddress.DomainNameLen);
  end
  else
  begin
    SocksResp^.AddressType := CN_SOCKS_ADDRESS_TYPE_IPV4;
    SocksResp^.BindAddress.IpV4Address := IP;
  end;
end;

function CnSetSocksResponseBindPort(const SocksResp: PCnSocksResponse; Port: Word): Integer;
var
  Len: Integer;
  PortAddr: PWORD;
begin
  Len := 0;

  case SocksResp^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      Len := 4;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      Len := 6;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksResp^.BindAddress.DomainNameLen + 1;
      end;
  end;

  if Len > 0 then
  begin
    PortAddr := PWORD(Integer(@(SocksResp^.BindAddress)) + Len);
    PortAddr^ := CnHostToNetworkWord(Port);
  end;
  Result := Len + 4 + SizeOf(Word);
end;

end.
