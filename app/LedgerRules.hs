{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module LedgerRules
  ( RuleCheck,
    ruleCheckByteExact,
    ruleCheckName,
    deserializeRule,
    reserializeRule,
    EraSpec,
    eraSpecName,
    eraSpecProtocolVersion,
    eraSpecRules,
    supportedEras,
    supportedEraNames,
    lookupEra,
    ruleNames,
    lookupRule,
  )
where

import Cardano.Base.IP (IPv4, IPv6)
import Cardano.Crypto.DSIGN (SignedDSIGN)
import Cardano.Crypto.KES (VerKeyKES)
import Cardano.Crypto.VRF (VerKeyVRF)
import Cardano.Ledger.Address (AccountAddress, Addr, Withdrawals)
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (PlutusPurpose), AsIx, CostModels)
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.BaseTypes
  ( Anchor,
    BlockNo,
    DnsName,
    EpochNo,
    Network,
    NonNegativeInterval,
    ProtVer,
    SlotNo,
    UnitInterval,
    Url,
  )
import Cardano.Ledger.BaseTypes.NonZero (NonZero)
import Cardano.Ledger.Binary
  ( Annotator,
    DecCBOR (decCBOR),
    DecCBORGroup (decCBORGroup),
    EncCBOR (encCBOR),
    EncCBORGroup (encCBORGroup),
    Version,
    decodeFull',
    decodeFullAnnotator,
    decodeRecordNamed,
    encodeListLen,
    getVersion64,
    serialize',
  )
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
  ( Constitution,
    GovAction,
    GovActionId,
    ProposalProcedure,
    Vote,
    Voter,
    VotingProcedure,
    VotingProcedures,
  )
import Cardano.Ledger.Conway.PParams (DRepVotingThresholds, PoolVotingThresholds)
import Cardano.Ledger.Core
  ( BlockBody,
    NativeScript,
    PParamsUpdate,
    Script,
    SubTx,
    TopTx,
    Tx,
    TxAuxData,
    TxBody,
    TxCert,
    TxOut,
    TxWits,
    Value,
    eraProtVerHigh,
  )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRep)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceInterval, AccountBalanceIntervals)
import Cardano.Ledger.Hashes
  ( EraIndependentTxBody,
    HASH,
    Hash,
    KeyRoleVRF (StakePoolVRF),
    ScriptHash,
    TxAuxDataHash,
    VRFVerKeyHash,
  )
import Cardano.Ledger.Keys
  ( BootstrapWitness,
    DSIGN,
    KeyHash,
    KeyRole (ColdCommitteeRole, DRepRole, HotCommitteeRole, Payment, StakePool, Staking, Witness),
    VKey,
    WitVKey,
  )
import Cardano.Ledger.Mary.Value (AssetName, MultiAsset, PolicyID)
import Cardano.Ledger.Metadata (Metadatum)
import Cardano.Ledger.Plutus.Data (BinaryData, Data, Datum)
import Cardano.Ledger.Plutus.ExUnits (ExUnits, Prices)
import Cardano.Ledger.Plutus.Language (Language, PlutusBinary)
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData)
import Cardano.Ledger.State (PoolMetadata, StakePoolRelay)
import Cardano.Ledger.TxIn (TxId, TxIn)
import Cardano.Protocol.Crypto (KES, StandardCrypto, VRF)
import Cardano.Protocol.Praos.BlockHeader (Header, HeaderBody)
import Cardano.Protocol.TPraos.OCert (KESPeriod, OCert)
import Data.ByteString qualified as BS
import Data.OSet.Strict (OSet)
import Data.Sequence.Strict (StrictSeq)
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Core.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Protocol.Binary.Annotator ()

data ConwaySingleRedeemer
  = ConwaySingleRedeemer
      !(PlutusPurpose AsIx ConwayEra)
      !(Data ConwayEra)
      !ExUnits

instance DecCBOR ConwaySingleRedeemer where
  decCBOR =
    decodeRecordNamed "Redeemer" (const 4) $
      ConwaySingleRedeemer <$> decCBORGroup <*> decCBOR <*> decCBOR

instance EncCBOR ConwaySingleRedeemer where
  encCBOR (ConwaySingleRedeemer purpose redeemerData exUnits) =
    encodeListLen 4
      <> encCBORGroup purpose
      <> encCBOR redeemerData
      <> encCBOR exUnits

-- | A rule whose type is read through an annotator.
--
-- A header keeps the bytes it decoded so its hash can be taken later, and the
-- ledger expresses that with @DecCBOR (Annotator a)@ rather than a plain
-- instance. The decoder is still the ledger's: only the entry point differs.
mkAnnotatorRuleCheck ::
  forall a.
  (DecCBOR (Annotator a), EncCBOR a) =>
  Version ->
  String ->
  RuleCheck
mkAnnotatorRuleCheck version name =
  RuleCheck
    { ruleCheckName = name,
      ruleCheckByteExact = name `elem` byteExactRuleNames,
      deserializeRule = decodeWith $ const (),
      reserializeRule = decodeWith $ serialize' version
    }
  where
    decodeWith :: (a -> b) -> BS.ByteString -> Either String b
    decodeWith transform bytes =
      case decodeFullAnnotator version (toText name) decCBOR (BS.fromStrict bytes) of
        Left err -> Left $ show err
        Right value -> Right $ transform value

decodeAs :: forall a b. (DecCBOR a) => Version -> (a -> b) -> BS.ByteString -> Either String b
decodeAs version transform bytes =
  case decodeFull' version bytes of
    Left err -> Left $ show err
    Right (value :: a) -> Right $ transform value

-- | Rules whose own bytes are a hash preimage. For those the encoding is part
-- of the format, since a different encoding is a different hash, so re-encoding
-- one of these samples must reproduce the original bytes exactly rather than
-- merely agree with them after normalization.
--
-- A rule belongs here only when the bytes of the item this rule encodes are
-- themselves hashed. Containing something that is hashed is not enough: a
-- @script@ is a tag beside a payload and a @datum_option@ selects between a
-- hash and an inline value, so what gets hashed is the payload, encoded by its
-- own rule, and the selector around it is free to be re-encoded. @cost_models@
-- is the case worth remembering, because its bytes genuinely do reach a hash,
-- but as the @language_views@ embedding inside the script integrity hash rather
-- than as the protocol-parameter map this rule encodes.
--
-- The ledger settles it for each type. A type built on @MemoBytes@ keeps the
-- bytes it decoded and re-emits them verbatim, since @encCBOR@ for a
-- @MemoBytes@ is @encodePreEncoded@ over the stored bytes, and a hash needs
-- exactly that. Everything else builds a fresh encoding, so a sample whose
-- original container form differs from the one the encoder writes cannot be
-- reproduced by anyone, and listing such a rule would demand the impossible.
--
-- @block@, @header_body@ and @transaction@ are the near misses: each wraps
-- memoized contents in a record it rebuilds on encode, so the parts keep their
-- bytes while the wrapper does not.
byteExactRuleNames :: [String]
byteExactRuleNames =
  [ "auxiliary_data",
    "header",
    "native_script",
    "plutus_data",
    "redeemers",
    "transaction_body",
    "transaction_witness_set"
  ]

data RuleCheck = RuleCheck
  { ruleCheckName :: !String,
    ruleCheckByteExact :: !Bool,
    deserializeRule :: BS.ByteString -> Either String (),
    reserializeRule :: BS.ByteString -> Either String BS.ByteString
  }

mkRuleCheck :: forall a. (DecCBOR a, EncCBOR a) => Version -> String -> RuleCheck
mkRuleCheck version name =
  RuleCheck
    { ruleCheckName = name,
      ruleCheckByteExact = name `elem` byteExactRuleNames,
      deserializeRule = decodeAs @a version $ const (),
      reserializeRule = decodeAs @a version $ serialize' version
    }

conwayRuleChecks :: [RuleCheck]
conwayRuleChecks =
  let version = eraProtVerHigh @ConwayEra
   in [ mkAnnotatorRuleCheck @(Block (Header StandardCrypto) ConwayEra) version "block",
        mkAnnotatorRuleCheck @(Header StandardCrypto) version "header",
        mkRuleCheck @(HeaderBody StandardCrypto) version "header_body",
        mkRuleCheck @(Tx TopTx ConwayEra) version "transaction",
        mkRuleCheck @(TxBody TopTx ConwayEra) version "transaction_body",
        mkRuleCheck @TxIn version "transaction_input",
        mkRuleCheck @(TxWits ConwayEra) version "transaction_witness_set",
        mkRuleCheck @(TxOut ConwayEra) version "transaction_output",
        mkRuleCheck @(Value ConwayEra) version "value",
        mkRuleCheck @(Script ConwayEra) version "script",
        mkRuleCheck @(Datum ConwayEra) version "datum_option",
        mkRuleCheck @(TxCert ConwayEra) version "certificate",
        mkRuleCheck @(OSet (TxCert ConwayEra)) version "certificates",
        mkRuleCheck @(Timelock ConwayEra) version "native_script",
        mkRuleCheck @(Data ConwayEra) version "plutus_data",
        mkRuleCheck @ConwaySingleRedeemer version "redeemer",
        mkRuleCheck @(Redeemers ConwayEra) version "redeemers",
        mkRuleCheck @(TxAuxData ConwayEra) version "auxiliary_data",
        mkRuleCheck @(Credential Staking) version "credential",
        mkRuleCheck @DRep version "drep",
        mkRuleCheck @StakePoolRelay version "relay",
        mkRuleCheck @(GovAction ConwayEra) version "gov_action",
        mkRuleCheck @(VotingProcedure ConwayEra) version "voting_procedure",
        mkRuleCheck @(ProposalProcedure ConwayEra) version "proposal_procedure",
        mkRuleCheck @(OSet (ProposalProcedure ConwayEra)) version "proposal_procedures",
        mkRuleCheck @(PParamsUpdate ConwayEra) version "protocol_param_update",
        mkRuleCheck @CostModels version "cost_models",
        mkRuleCheck @Voter version "voter",
        mkRuleCheck @Metadatum version "metadatum",
        mkRuleCheck @DRepVotingThresholds version "drep_voting_thresholds",
        mkRuleCheck @PoolVotingThresholds version "pool_voting_thresholds",
        mkRuleCheck @ExUnits version "ex_units",
        mkRuleCheck @Anchor version "anchor",
        mkRuleCheck @GovActionId version "gov_action_id",
        mkRuleCheck @(Constitution ConwayEra) version "constitution",
        mkRuleCheck @(OCert StandardCrypto) version "operational_cert",
        mkRuleCheck @BootstrapWitness version "bootstrap_witness",
        mkRuleCheck @(WitVKey Witness) version "vkeywitness",
        mkRuleCheck @Withdrawals version "withdrawals",
        mkRuleCheck @MultiAsset version "mint",
        mkRuleCheck @Addr version "address",
        mkRuleCheck @AccountAddress version "reward_account",
        mkRuleCheck @ProtVer version "protocol_version",
        mkRuleCheck @Coin version "coin",
        mkRuleCheck @EpochNo version "epoch",
        mkRuleCheck @SlotNo version "slot",
        mkRuleCheck @BlockNo version "block_number",
        mkRuleCheck @Network version "network_id",
        mkRuleCheck @Url version "url",
        mkRuleCheck @DnsName version "dns_name",
        mkRuleCheck @UnitInterval version "unit_interval",
        mkRuleCheck @NonNegativeInterval version "nonnegative_interval",
        mkRuleCheck @TxId version "transaction_id",
        mkRuleCheck @ScriptHash version "script_hash",
        mkRuleCheck @(KeyHash Payment) version "addr_keyhash",
        mkRuleCheck @(KeyHash StakePool) version "pool_keyhash",
        mkRuleCheck @(VRFVerKeyHash StakePoolVRF) version "vrf_keyhash",
        mkRuleCheck @TxAuxDataHash version "auxiliary_data_hash",
        mkRuleCheck @PolicyID version "policy_id",
        mkRuleCheck @AssetName version "asset_name",
        mkRuleCheck @(Credential Staking) version "stake_credential",
        mkRuleCheck @(Credential DRepRole) version "drep_credential",
        mkRuleCheck @(Credential ColdCommitteeRole) version "committee_cold_credential",
        mkRuleCheck @(Credential HotCommitteeRole) version "committee_hot_credential",
        mkRuleCheck @Vote version "vote",
        mkRuleCheck @Prices version "ex_unit_prices",
        mkRuleCheck @PoolMetadata version "pool_metadata",
        mkRuleCheck @KESPeriod version "kes_period",
        mkRuleCheck @Word16 version "transaction_index",
        mkRuleCheck @(VKey Witness) version "vkey",
        -- The nonempty_set and nonempty_oset rules carry no root of their own.
        -- No ledger type decodes them: Set and OSet accept an empty container,
        -- and the non-emptiness is enforced one level up, by the decoder of the
        -- field holding them. A root here would exercise the wrong decoder and
        -- leave its own lower bound permanently unmet.
        mkRuleCheck @MultiAsset version "%multiasset<positive_coin>",
        mkRuleCheck @(AlonzoTxOut ConwayEra) version "alonzo_transaction_output",
        mkRuleCheck @Language version "language",
        mkRuleCheck @(AllegraTxAuxData ConwayEra) version "auxiliary_data_array",
        mkRuleCheck @(StrictSeq (Timelock ConwayEra)) version "auxiliary_scripts",
        mkRuleCheck @ScriptHash version "hash28",
        mkRuleCheck @TxId version "hash32",
        mkRuleCheck @TxId version "script_data_hash",
        mkRuleCheck @ScriptHash version "guardrails_script_hash",
        mkRuleCheck @Version version "major_protocol_version",
        mkRuleCheck @(NonZero Coin) version "positive_coin",
        mkRuleCheck @Int64 version "positive_int64",
        mkRuleCheck @Int64 version "negative_int64",
        mkRuleCheck @(NonZero Int) version "nonzero_int64",
        mkRuleCheck @Integer version "big_int",
        mkRuleCheck @ByteString version "bounded_bytes",
        mkRuleCheck @IPv4 version "ipv4",
        mkRuleCheck @IPv6 version "ipv6",
        mkRuleCheck @(VerKeyKES (KES StandardCrypto)) version "kes_vkey",
        mkRuleCheck @(VerKeyVRF (VRF StandardCrypto)) version "vrf_vkey",
        mkRuleCheck @(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody)) version "signature",
        -- Not a signature, but the rule is the same shape: sixty four bytes,
        -- and this is the only type at hand that holds the decoder to it.
        mkRuleCheck @(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody)) version "signkey_kes",
        mkRuleCheck @(Data ConwayEra) version "%constr<plutus_data>",
        mkRuleCheck @PlutusBinary version "plutus_v1_script",
        mkRuleCheck @PlutusBinary version "plutus_v2_script",
        mkRuleCheck @PlutusBinary version "plutus_v3_script",
        mkRuleCheck @(BinaryData ConwayEra) version "data",
        mkRuleCheck @(ShelleyTxAuxData ConwayEra) version "metadata",
        mkRuleCheck @(AlonzoTxAuxData ConwayEra) version "auxiliary_data_map",
        mkRuleCheck @(VotingProcedures ConwayEra) version "voting_procedures",
        mkRuleCheck @(BabbageTxOut ConwayEra) version "babbage_transaction_output"
      ]

dijkstraRuleChecks :: [RuleCheck]
dijkstraRuleChecks =
  let version = eraProtVerHigh @DijkstraEra
   in [ mkAnnotatorRuleCheck @(Block (Header StandardCrypto) DijkstraEra) version "block",
        mkAnnotatorRuleCheck @(Header StandardCrypto) version "header",
        mkRuleCheck @(HeaderBody StandardCrypto) version "header_body",
        mkRuleCheck @(BlockBody DijkstraEra) version "block_body",
        mkRuleCheck @(Tx TopTx DijkstraEra) version "transaction",
        mkRuleCheck @(TxBody TopTx DijkstraEra) version "transaction_body",
        mkRuleCheck @(TxBody SubTx DijkstraEra) version "sub_transaction_body",
        mkRuleCheck @TxIn version "transaction_input",
        mkRuleCheck @(TxWits DijkstraEra) version "transaction_witness_set",
        mkRuleCheck @(TxOut DijkstraEra) version "transaction_output",
        mkRuleCheck @(Value DijkstraEra) version "value",
        mkRuleCheck @(Script DijkstraEra) version "script",
        mkRuleCheck @(Datum DijkstraEra) version "datum_option",
        mkRuleCheck @(TxCert DijkstraEra) version "certificate",
        mkRuleCheck @(OSet (TxCert DijkstraEra)) version "certificates",
        mkRuleCheck @(NativeScript DijkstraEra) version "native_script",
        mkRuleCheck @(Data DijkstraEra) version "plutus_data",
        mkRuleCheck @(Redeemers DijkstraEra) version "redeemers",
        mkRuleCheck @(TxAuxData DijkstraEra) version "auxiliary_data",
        mkRuleCheck @(Credential Staking) version "credential",
        mkRuleCheck @DRep version "drep",
        mkRuleCheck @StakePoolRelay version "relay",
        mkRuleCheck @(GovAction DijkstraEra) version "gov_action",
        mkRuleCheck @(VotingProcedure DijkstraEra) version "voting_procedure",
        mkRuleCheck @(VotingProcedures DijkstraEra) version "voting_procedures",
        mkRuleCheck @(ProposalProcedure DijkstraEra) version "proposal_procedure",
        mkRuleCheck @(OSet (ProposalProcedure DijkstraEra)) version "proposal_procedures",
        mkRuleCheck @(PParamsUpdate DijkstraEra) version "protocol_param_update",
        mkRuleCheck @CostModels version "cost_models",
        mkRuleCheck @(AccountBalanceInterval DijkstraEra) version "account_balance_interval",
        mkRuleCheck @(AccountBalanceIntervals DijkstraEra) version "account_balance_intervals",
        mkRuleCheck @Voter version "voter",
        mkRuleCheck @Metadatum version "metadatum",
        mkRuleCheck @DRepVotingThresholds version "drep_voting_thresholds",
        mkRuleCheck @PoolVotingThresholds version "pool_voting_thresholds",
        mkRuleCheck @ExUnits version "ex_units",
        mkRuleCheck @Anchor version "anchor",
        mkRuleCheck @GovActionId version "gov_action_id",
        mkRuleCheck @(Constitution DijkstraEra) version "constitution",
        mkRuleCheck @(OCert StandardCrypto) version "operational_cert",
        mkRuleCheck @BootstrapWitness version "bootstrap_witness",
        mkRuleCheck @(WitVKey Witness) version "vkeywitness",
        mkRuleCheck @Withdrawals version "withdrawals",
        mkRuleCheck @MultiAsset version "mint",
        mkRuleCheck @Addr version "address",
        mkRuleCheck @AccountAddress version "reward_account",
        mkRuleCheck @ProtVer version "protocol_version",
        mkRuleCheck @Coin version "coin",
        mkRuleCheck @EpochNo version "epoch",
        mkRuleCheck @SlotNo version "slot",
        mkRuleCheck @BlockNo version "block_number",
        mkRuleCheck @Network version "network_id",
        mkRuleCheck @Url version "url",
        mkRuleCheck @DnsName version "dns_name",
        mkRuleCheck @UnitInterval version "unit_interval",
        mkRuleCheck @NonNegativeInterval version "nonnegative_interval",
        mkRuleCheck @TxId version "transaction_id",
        mkRuleCheck @ScriptHash version "script_hash",
        mkRuleCheck @(KeyHash Payment) version "addr_keyhash",
        mkRuleCheck @(KeyHash StakePool) version "pool_keyhash",
        mkRuleCheck @(VRFVerKeyHash StakePoolVRF) version "vrf_keyhash",
        mkRuleCheck @TxAuxDataHash version "auxiliary_data_hash",
        mkRuleCheck @PolicyID version "policy_id",
        mkRuleCheck @AssetName version "asset_name",
        mkRuleCheck @(Credential Staking) version "stake_credential",
        mkRuleCheck @(Credential DRepRole) version "drep_credential",
        mkRuleCheck @(Credential ColdCommitteeRole) version "committee_cold_credential",
        mkRuleCheck @(Credential HotCommitteeRole) version "committee_hot_credential",
        mkRuleCheck @Vote version "vote",
        mkRuleCheck @Prices version "ex_unit_prices",
        mkRuleCheck @PoolMetadata version "pool_metadata",
        mkRuleCheck @KESPeriod version "kes_period",
        mkRuleCheck @Word16 version "transaction_index",
        mkRuleCheck @(VKey Witness) version "vkey",
        -- The nonempty_set and nonempty_oset rules carry no root of their own.
        -- No ledger type decodes them: Set and OSet accept an empty container,
        -- and the non-emptiness is enforced one level up, by the decoder of the
        -- field holding them. A root here would exercise the wrong decoder and
        -- leave its own lower bound permanently unmet.
        mkRuleCheck @MultiAsset version "%multiasset<positive_coin>",
        mkRuleCheck @(AlonzoTxOut DijkstraEra) version "alonzo_transaction_output",
        mkRuleCheck @Language version "language",
        mkRuleCheck @(AllegraTxAuxData DijkstraEra) version "auxiliary_data_array",
        mkRuleCheck @(StrictSeq (Timelock DijkstraEra)) version "auxiliary_scripts",
        mkRuleCheck @ScriptHash version "hash28",
        mkRuleCheck @TxId version "hash32",
        mkRuleCheck @TxId version "script_data_hash",
        mkRuleCheck @ScriptHash version "guardrails_script_hash",
        mkRuleCheck @Version version "major_protocol_version",
        mkRuleCheck @(NonZero Coin) version "positive_coin",
        mkRuleCheck @Int64 version "positive_int64",
        mkRuleCheck @Int64 version "negative_int64",
        mkRuleCheck @(NonZero Int) version "nonzero_int64",
        mkRuleCheck @Integer version "big_int",
        mkRuleCheck @ByteString version "bounded_bytes",
        mkRuleCheck @IPv4 version "ipv4",
        mkRuleCheck @IPv6 version "ipv6",
        mkRuleCheck @(VerKeyKES (KES StandardCrypto)) version "kes_vkey",
        mkRuleCheck @(VerKeyVRF (VRF StandardCrypto)) version "vrf_vkey",
        mkRuleCheck @(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody)) version "signature",
        -- Not a signature, but the rule is the same shape: sixty four bytes,
        -- and this is the only type at hand that holds the decoder to it.
        mkRuleCheck @(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody)) version "signkey_kes",
        mkRuleCheck @(Data DijkstraEra) version "%constr<plutus_data>",
        mkRuleCheck @PlutusBinary version "plutus_v1_script",
        mkRuleCheck @PlutusBinary version "plutus_v2_script",
        mkRuleCheck @PlutusBinary version "plutus_v3_script",
        mkRuleCheck @(BinaryData DijkstraEra) version "data",
        mkRuleCheck @(ShelleyTxAuxData DijkstraEra) version "metadata",
        mkRuleCheck @(AlonzoTxAuxData DijkstraEra) version "auxiliary_data_map",
        mkRuleCheck @(BabbageTxOut DijkstraEra) version "babbage_transaction_output"
      ]

data EraSpec = EraSpec
  { eraSpecName :: !String,
    -- | The protocol version the era's decoders run at, as a conformance
    -- report spells it. Taken from the ledger rather than written out here, so
    -- a report cannot claim a version the samples were not decoded against.
    eraSpecProtocolVersion :: !String,
    eraSpecRules :: ![RuleCheck]
  }

protocolVersionName :: Version -> String
protocolVersionName version = show (getVersion64 version) <> ".0"

supportedEras :: [EraSpec]
supportedEras =
  [ EraSpec "conway" (protocolVersionName $ eraProtVerHigh @ConwayEra) conwayRuleChecks,
    EraSpec "dijkstra" (protocolVersionName $ eraProtVerHigh @DijkstraEra) dijkstraRuleChecks
  ]

supportedEraNames :: [String]
supportedEraNames = map eraSpecName supportedEras

lookupNamed :: String -> (a -> String) -> [a] -> String -> Either String a
lookupNamed description getName values requested =
  case find ((== requested) . getName) values of
    Just value -> Right value
    Nothing ->
      Left $
        "unknown "
          <> description
          <> " '"
          <> requested
          <> "': expected "
          <> intercalate ", " (map getName values)

lookupEra :: String -> Either String EraSpec
lookupEra = lookupNamed "era" eraSpecName supportedEras

ruleNames :: EraSpec -> [String]
ruleNames = map ruleCheckName . eraSpecRules

lookupRule :: EraSpec -> String -> Either String RuleCheck
lookupRule era = lookupNamed (eraSpecName era <> " rule") ruleCheckName (eraSpecRules era)
