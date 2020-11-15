{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.ByteString.Mp4.Boxes.BaseDescriptor where

import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.Mp4.Boxes.Expandable

-- * Static base constructor

-- | Abstract class of /descriptors/ as recognized by ISO/IEC 14496-1 (Systems).
-- A specifc descriptor is identified by the 'ClassTag'.
data Descriptor :: ClassTag n -> Type where
  MkDescriptor ::BitRecord -> Descriptor tag

-- TODO ok... this fixed the current problem in DecoderSpecificInfo .. but remove this instances ... or the above ... or ... I dunno

data BitRecordOfDescriptor :: Extends (Descriptor c -> BitRecord)

type instance
  Apply BitRecordOfDescriptor ('MkDescriptor body :: Descriptor (tag :: ClassTag tagInd)) =
   FieldU8 := tagInd
   .+: From (StaticExpandableContent body)

type family GetClassTag (c :: ClassTag n) :: Nat where
  GetClassTag (c :: ClassTag n) = n

-- | Base Descriptor Class Tags TODO rename to xxxTag
data ClassTag (tag :: Nat) where
  ObjectDescr                      ::ClassTag 0x01
  InitialObjectDescr               ::ClassTag 0x02
  ES_Descr                         ::ClassTag 0x03
  DecoderConfigDescr               ::ClassTag 0x04
  DecSpecificInfo                  ::ClassTag 0x05
  SLConfigDescr                    ::ClassTag 0x06
  ContentIdentDescr                ::ClassTag 0x07
  SupplContentIdentDescr           ::ClassTag 0x08
  IPI_DescrPointer                 ::ClassTag 0x09
  IPMP_DescrPointer                ::ClassTag 0x0A
  IPMP_Descr                       ::ClassTag 0x0B
  QoS_Descr                        ::ClassTag 0x0C
  RegistrationDescr                ::ClassTag 0x0D
  ES_ID_Ref                        ::ClassTag 0x0F
  MP4_IOD_                         ::ClassTag 0x10
  MP4_OD_                          ::ClassTag 0x11
  IPL_DescrPointerRef              ::ClassTag 0x12
  ExtensionProfileLevelDescr       ::ClassTag 0x13
  ProfileLevelIndicationIndexDescr ::ClassTag 0x14
  ContentClassificationDescr       ::ClassTag 0x40
  KeyWordDescr                     ::ClassTag 0x41
  RatingDescr                      ::ClassTag 0x42
  LanguageDescr                    ::ClassTag 0x43
  ShortTextualDescr                ::ClassTag 0x44
  ExpandedTextualDescr             ::ClassTag 0x45
  ContentCreatorNameDescr          ::ClassTag 0x46
  ContentCreationDateDescr         ::ClassTag 0x47
  OCICreatorNameDescr              ::ClassTag 0x48
  OCICreationDateDescr             ::ClassTag 0x49
  SmpteCameraPositionDescr         ::ClassTag 0x4A
  SegmentDescr                     ::ClassTag 0x4B
  MediaTimeDescr                   ::ClassTag 0x4C
  IPMP_ToolsListDescr              ::ClassTag 0x60
  IPMP_Tool                        ::ClassTag 0x61
  M4MuxTimingDescr                 ::ClassTag 0x62
  M4MuxCodeTableDescr              ::ClassTag 0x63
  ExtSLConfigDescr                 ::ClassTag 0x64
  M4MuxBufferSizeDescr             ::ClassTag 0x65
  M4MuxIdentDescr                  ::ClassTag 0x66
  DependencyPointer                ::ClassTag 0x67
  DependencyMarker                 ::ClassTag 0x68
  M4MuxChannelDescr                ::ClassTag 0x69
  ExtDescrTag :: (forall (n :: Nat) . (0x6A <= n, n <= 0xFE) =>  ClassTag n)
  OCIDescrTag :: (forall (n :: Nat) . (0x40 <= n, n <= 0x5F) =>  ClassTag n)
