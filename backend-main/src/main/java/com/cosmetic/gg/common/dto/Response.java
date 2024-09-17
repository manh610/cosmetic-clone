package com.cosmetic.gg.common.dto;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

import javax.validation.constraints.Pattern;

import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.common.model.Error;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Response<T> {
	@JsonFormat(shape = JsonFormat.Shape.BOOLEAN)
	private Boolean status = false;
	
	private LocalDateTime timestamp = LocalDateTime.now();
	
	private String transId = UUID.randomUUID().toString();
	
	@JsonFormat(shape = JsonFormat.Shape.NUMBER_INT)
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@Pattern(message = "Total item must be number", regexp="^[0-9]*$")
	private Object totalItem;
	
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty(value = "errors")
	private Error[] errors;
	
	@JsonInclude(JsonInclude.Include.NON_NULL)
	private Object data;
	
	public Response<T> error(String transId, Error... errors) {
		return new Response<>(
	      this.status = false,
	      this.timestamp = LocalDateTime.now(),
	      this.transId = StringUtils.isNullOrEmpty(transId) ? UUID.randomUUID().toString() : transId,
	      this.totalItem = null,
	      this.errors = errors,
	      this.data = null
	    );
	}
	
	public Response<T> error(String transId, List<Error> errors) {
		return new Response<>(
	        this.status = false,
	        this.timestamp = LocalDateTime.now(),
	        this.transId = StringUtils.isNullOrEmpty(transId) ? UUID.randomUUID().toString() : transId,
	        this.totalItem = null,
	        this.errors = errors == null ? null : errors.toArray(new Error[0]),
	        this.data = null
	    );
	}
	
	public Response<T> success(String transId, T data) {
		return new Response<>(
	        this.status = true,
	        this.timestamp = LocalDateTime.now(),
	        this.transId = StringUtils.isNullOrEmpty(transId) ? UUID.randomUUID().toString() : transId,
	        this.totalItem = null,
	        this.errors = null,
	        this.data = data
	    );
	}
	
	public Response<T> success(String transId, int totalItem, T data) {
		return new Response<>(
	        this.status = true,
	        this.timestamp = LocalDateTime.now(),
	        this.transId = StringUtils.isNullOrEmpty(transId) ? UUID.randomUUID().toString() : transId,
	        this.totalItem = totalItem,
	        this.errors = null,
	        this.data = data
	    );
	}
}
