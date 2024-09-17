package com.cosmetic.gg.common.utils.json;

import com.cosmetic.gg.common.utils.object.ObjectMapperUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.node.MissingNode;

import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

@Slf4j
public abstract class Json {
  private static final ObjectMapper objectMapper = ObjectMapperUtils.defaultInstance();

  public static JsonNode createObjectNode() {
    return objectMapper.createObjectNode();
  }

  public static JsonNode parse(String str) throws IOException {
    try {
      return objectMapper.readTree(str);
    } catch (JsonProcessingException e) {
      log.error(e.getMessage());
    }
    return null;
  }

  public static JsonNode parse(Object obj) throws IOException {
    try {
      ObjectWriter ow = objectMapper.writer().withDefaultPrettyPrinter();
      ow.writeValueAsString(obj);
      return objectMapper.readTree(ow.writeValueAsString(obj));
    } catch (JsonProcessingException e) {
      log.error(e.getMessage());
    }
    return null;
  }

  public static JsonNode createMissingNode() {
    try {
      return objectMapper.readValue("", JsonNode.class);
    } catch (Exception ex) {
      log.error(ex.getMessage());
    }

    return MissingNode.getInstance();
  }

  public static JsonNode parse(File file) throws IOException {
    try {
      return objectMapper.readTree(file);
    } catch (JsonProcessingException e) {
      log.error(e.getMessage());
    }
    return null;
  }

  public static JsonNode parse(InputStream inputStream) throws IOException {
    try {
      return objectMapper.readTree(inputStream);
    } catch (JsonProcessingException e) {
      log.error(e.getMessage());
    }
    return null;
  }

//  public static <T> T getWithJsonPath(Object jsonDataSource, String jsonPath) {
//    return JsonPath.parse(jsonDataSource).read(jsonPath);
//  }
//
//  public static <T> T getWithJsonPath(String jsonDataSource, String jsonPath) {
//    return JsonPath.parse(jsonDataSource).read(jsonPath);
//  }
//
//  public static <T> T getWithJsonPath(File jsonDataSource, String jsonPath) throws IOException {
//    return JsonPath.parse(jsonDataSource).read(jsonPath);
//  }
//
//  public static <T> T getWithJsonPath(Object jsonDataSource, String jsonPath, Class<T> type) {
//    return JsonPath.parse(jsonDataSource).read(jsonPath, type);
//  }
//
//  public static <T> T getWithJsonPath(String jsonDataSource, String jsonPath, Class<T> type) {
//    return JsonPath.parse(jsonDataSource).read(jsonPath, type);
//  }
//
//  public static <T> T getWithJsonPath(File jsonDataSource, String jsonPath, Class<T> type) throws IOException {
//    return JsonPath.parse(jsonDataSource).read(jsonPath, type);
//  }
//
//  public static <T> T getWithJsonPath(Object jsonDataSource, String jsonPath, TypeRef<T> typeRef) {
//    return JsonPath.parse(jsonDataSource).read(jsonPath, typeRef);
//  }
//
//  public static <T> T getWithJsonPath(String jsonDataSource, String jsonPath, TypeRef<T> typeRef) {
//    return JsonPath.parse(jsonDataSource).read(jsonPath, typeRef);
//  }
//
//  public static <T> T getWithJsonPath(File jsonDataSource, String jsonPath, TypeRef<T> typeRef) throws IOException {
//    return JsonPath.parse(jsonDataSource).read(jsonPath, typeRef);
//  }
//
//  public static JsonNode getNode(Object jsonDataSource, String jsonPath) {
//    return objectMapper.convertValue(getWithJsonPath(jsonDataSource, jsonPath), JsonNode.class);
//  }
//
//  public static JsonNode getNode(String jsonDataSource, String jsonPath) {
//    return objectMapper.convertValue(getWithJsonPath(jsonDataSource, jsonPath), JsonNode.class);
//  }
//
//  public static JsonNode getNode(File jsonDataSource, String jsonPath) throws IOException {
//    return objectMapper.convertValue(getWithJsonPath(jsonDataSource, jsonPath), JsonNode.class);
//  }
//
//  public static String toJson(Object value) {
//    try {
//      return objectMapper.writeValueAsString(value);
//    } catch (JsonProcessingException e) {
//      log.error("stringToDate error: {}", e.getMessage());
//      return Strings.EMPTY;
//    }
//  }

  public static <T> T fromJson(JsonNode node, Class<T> clazz) throws JsonProcessingException {
    T target = null;
    try {
      target = objectMapper.treeToValue(node, clazz);
    } catch (JsonProcessingException e) {
      log.error(e.getMessage());
    }
    return target;
  }

  public static <T> T fromJson(String json, TypeReference<T> type) {
    try {
      return objectMapper.readValue(json, type);
    } catch (JsonProcessingException e) {
      log.error(e.getMessage());
    }
    return null;
  }

  public static <T> T fromJson(String data, Class<T> clazz) throws IOException {
    JsonNode _data = parse(data);
    return fromJson(_data, clazz);
  }

  public static <T> T cast(Object obj, Class<T> clazz) {
    if (null == obj)
      return null;

    return objectMapper.convertValue(obj, clazz);
  }

  public static <T> T cast(Object obj, TypeReference<T> type) {
    if (null == obj)
      return null;

    return objectMapper.convertValue(obj, type);
  }

  public static <T> T cast(Object obj) {
    if (null == obj)
      return null;

    return objectMapper.convertValue(obj, new TypeReference<T>() {
    });
  }
}
