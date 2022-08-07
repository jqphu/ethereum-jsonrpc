use crate::prelude::*;
use serde::de::{self, Visitor};
use std::fmt;
use std::marker::PhantomData;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AccessListEntry {
    pub address: Address,
    pub storage_keys: Vec<H256>,
}

#[derive(PartialEq, Debug, Copy, Clone, Default)]
pub struct LegacyType;
impl MessageCallTag for LegacyType {
    const TAG: &'static str = "0x00";
}

trait MessageCallTag {
    const TAG: &'static str;
}

#[derive(PartialEq, Debug, Copy, Clone, Default)]
pub struct TagWrapper<T>(T);

impl<T> Serialize for TagWrapper<T>
where
    T: MessageCallTag,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(T::TAG)
    }
}

#[derive(Default)]
struct MessageTagVisitor<T>(PhantomData<T>);
impl<'de, T> Visitor<'de> for MessageTagVisitor<T>
where
    T: MessageCallTag + Default,
{
    type Value = TagWrapper<T>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "expected the tag type {}", T::TAG)
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        if value != T::TAG {
            Err(de::Error::custom(format!("expected `{}`", T::TAG)))
        } else {
            Ok(TagWrapper::<T>::default())
        }
    }
}

impl<'de, T> Deserialize<'de> for TagWrapper<T>
where
    T: MessageCallTag + Default,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(MessageTagVisitor::<T>::default())
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Default)]
pub struct EIP2930Type;
impl EIP2930Type {
    const TAG: &'static str = "0x01";
}

impl Serialize for EIP2930Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(EIP2930Type::TAG)
    }
}

struct EIP2930TypeVisitor;
impl<'de> Visitor<'de> for EIP2930TypeVisitor {
    type Value = EIP2930Type;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "expected the tag type {}", EIP2930Type::TAG)
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        if value != EIP2930Type::TAG {
            Err(de::Error::custom(format!(
                "expected `{}`",
                EIP2930Type::TAG
            )))
        } else {
            Ok(EIP2930Type)
        }
    }
}

impl<'de> Deserialize<'de> for EIP2930Type {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(EIP2930TypeVisitor)
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Default)]
pub struct EIP1559Type;
impl EIP1559Type {
    const TAG: &'static str = "0x02";
}

impl Serialize for EIP1559Type {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(EIP1559Type::TAG)
    }
}

struct EIP1559TypeVisitor;
impl<'de> Visitor<'de> for EIP1559TypeVisitor {
    type Value = EIP1559Type;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "expected the tag type {}", EIP2930Type::TAG)
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        if value != EIP1559Type::TAG {
            Err(de::Error::custom(format!(
                "expected `{}`",
                EIP1559Type::TAG
            )))
        } else {
            Ok(EIP1559Type)
        }
    }
}

impl<'de> Deserialize<'de> for EIP1559Type {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(EIP1559TypeVisitor)
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
#[serde(untagged, deny_unknown_fields)]
pub enum MessageCall {
    #[serde(rename_all = "camelCase")]
    Legacy {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        tag: Option<TagWrapper<LegacyType>>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        from: Option<Address>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        to: Option<Address>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        gas: Option<U64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        gas_price: Option<U256>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        value: Option<U256>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        data: Option<Bytes>,
    },
    #[serde(rename_all = "camelCase")]
    EIP2930 {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        tag: Option<EIP2930Type>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        from: Option<Address>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        to: Option<Address>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        gas: Option<U64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        gas_price: Option<U256>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        value: Option<U256>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        data: Option<Bytes>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        access_list: Option<Vec<AccessListEntry>>,
    },
    #[serde(rename_all = "camelCase")]
    EIP1559 {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        tag: Option<EIP1559Type>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        from: Option<Address>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        to: Option<Address>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        gas: Option<U64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        max_fee_per_gas: Option<U256>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        max_priority_fee_per_gas: Option<U256>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        value: Option<U256>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        data: Option<Bytes>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        access_list: Option<Vec<AccessListEntry>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", deny_unknown_fields)]
pub enum TransactionMessage {
    #[serde(rename = "0x0")]
    #[serde(rename_all = "camelCase")]
    Legacy {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        chain_id: Option<U64>,
        nonce: U64,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        to: Option<Address>,
        gas: U64,
        gas_price: U256,
        value: U256,
        input: Bytes,
    },
    #[serde(rename = "0x1")]
    #[serde(rename_all = "camelCase")]
    EIP2930 {
        chain_id: U64,
        nonce: U64,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        to: Option<Address>,
        gas: U64,
        gas_price: U256,
        value: U256,
        input: Bytes,
        access_list: Vec<AccessListEntry>,
    },
    #[serde(rename = "0x2")]
    #[serde(rename_all = "camelCase")]
    EIP1559 {
        chain_id: U64,
        nonce: U64,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        to: Option<Address>,
        gas: U64,
        max_fee_per_gas: U256,
        max_priority_fee_per_gas: U256,
        value: U256,
        input: Bytes,
        access_list: Vec<AccessListEntry>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Transaction {
    #[serde(flatten)]
    pub message: TransactionMessage,
    /// RLP encoded representation of the transaction.
    pub v: U64,
    pub r: H256,
    pub s: H256,

    pub from: Address,
    pub hash: H256,
    pub transaction_index: Option<U64>,
    pub block_number: Option<U64>,
    pub block_hash: Option<H256>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
/// Tx is either a transaction or a transaction hash.
pub enum Tx {
    /// Transaction.
    Transaction(Box<Transaction>),
    /// Transaction hash.
    Hash(H256),
}

#[cfg(test)]
mod tests {
    use super::*;
    use hex_literal::hex;
    use serde_json::json;

    #[test]
    fn test_ser_de_hexbytes_option() {
        let call_data = MessageCall::Legacy {
            tag: None,
            from: None,
            to: Some(Address::from([0; 20])),
            gas: None,
            gas_price: None,
            value: None,
            data: None,
        };
        let hexstring = json!({
            "to":"0x0000000000000000000000000000000000000000",
        });
        assert_eq!(serde_json::to_value(&call_data).unwrap(), hexstring);
        assert_eq!(
            serde_json::from_value::<MessageCall>(hexstring).unwrap(),
            call_data
        );

        let call_data_with_data = MessageCall::Legacy {
            tag: None,
            from: None,
            to: Some(Address::from([0; 20])),
            gas: None,
            gas_price: None,
            value: None,
            data: Some(Bytes::from(&b"Hello Akula"[..])),
        };

        let hexstring_with_data = json!({
            "to":"0x0000000000000000000000000000000000000000",
            "data":"0x48656c6c6f20416b756c61",
        });
        assert_eq!(
            serde_json::to_value(&call_data_with_data).unwrap(),
            hexstring_with_data
        );
        assert_eq!(
            serde_json::from_value::<MessageCall>(hexstring_with_data).unwrap(),
            call_data_with_data
        );
    }

    #[test]
    fn test_deserialize_with_tag() {
        let call_data = MessageCall::Legacy {
            tag: Some(TagWrapper(LegacyType)),
            from: None,
            to: Some(Address::from([0; 20])),
            gas: None,
            gas_price: None,
            value: None,
            data: None,
        };

        let hexstring = json!({
            "tag": "0x00",
            "to":"0x0000000000000000000000000000000000000000",
        });

        assert_eq!(
            serde_json::from_value::<MessageCall>(hexstring).unwrap(),
            call_data,
        );
    }

    #[test]
    fn test_tx_ser() {
        let tx = Transaction {
            message: TransactionMessage::Legacy {
                chain_id: Some(2_u64.into()),
                nonce: 12_u64.into(),
                gas: 21000_u64.into(),
                gas_price: 20_000_000_000_u64.into(),
                to: Some(hex!("727fc6a68321b754475c668a6abfb6e9e71c169a").into()),
                value: 10.as_u256() * 1_000_000_000 * 1_000_000_000,
                input: hex!("a9059cbb000000000213ed0f886efd100b67c7e4ec0a85a7d20dc971600000000000000000000015af1d78b58c4000").to_vec().into(),
            },
            v: 40_u64.into(),
            r: hex!("be67e0a07db67da8d446f76add590e54b6e92cb6b8f9835aeb67540579a27717").into(),
            s: hex!("2d690516512020171c1ec870f6ff45398cc8609250326be89915fb538e7bd718").into(),
            from: Address::repeat_byte(0xAA),
            hash: H256::repeat_byte(0xBB),
            transaction_index: Some(0x42.into()),
            block_hash: None,
            block_number: None,
        };
        let serialized = json!({
            "type": "0x0",
            "chainId": "0x2",
            "nonce": "0xc",
            "to": "0x727fc6a68321b754475c668a6abfb6e9e71c169a",
            "gas": "0x5208",
            "gasPrice":"0x4a817c800",
            "value":"0x8ac7230489e80000",
            "input":"0xa9059cbb000000000213ed0f886efd100b67c7e4ec0a85a7d20dc971600000000000000000000015af1d78b58c4000",
            "v":"0x28",
            "r":"0xbe67e0a07db67da8d446f76add590e54b6e92cb6b8f9835aeb67540579a27717",
            "s":"0x2d690516512020171c1ec870f6ff45398cc8609250326be89915fb538e7bd718",
            "from":"0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            "hash":"0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
            "transactionIndex":"0x42",
            "blockHash": null,
            "blockNumber": null,
        });

        assert_eq!(serde_json::to_value(&tx).unwrap(), serialized);
        assert_eq!(
            serde_json::from_value::<Transaction>(serialized).unwrap(),
            tx
        );
    }
}
