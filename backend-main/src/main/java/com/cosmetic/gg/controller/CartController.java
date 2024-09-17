package com.cosmetic.gg.controller;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.entity.product.Cart;
import com.cosmetic.gg.service.product.CartService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/cart")
@AllArgsConstructor
@CrossOrigin
public class CartController {

	private final CartService cartService;
	
	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<ProductItemResponse> products = cartService.search(id);
			return new ResponseEntity<>(
		        new Response<List<ProductItemResponse>>().success(
		          transId,
		          products.size(),
		          products
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> addProduct(
			@RequestBody @Valid Cart cart,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			cart.setId("");
			
			List<Error> errors = cartService.validator(cart);
			if(!errors.isEmpty())
				return new ResponseEntity<>(
					new Response<Error[]>().error(
							transId,
							errors.toArray(new Error[0])
					),
					HttpStatus.BAD_REQUEST
				);
			
			Cart cartEntity = cartService.addProduct(cart);
			if (Objects.isNull(cartEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
				new Response<Cart>().success(
						transId,
						cartEntity
						),
				HttpStatus.CREATED
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/{id}/{quantity}",
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> updateProduct(
			@PathVariable(value = "id") String id,
			@PathVariable(value = "quantity") Integer quantity,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {					
			Cart cartEntity = cartService.updateProduct(id, quantity);
			if (Objects.isNull(cartEntity)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<Cart>().success(
							transId,
							cartEntity
							),
					HttpStatus.OK
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@DeleteMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> removeProduct(
			@PathVariable(value = "id") String id,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = cartService.removeProduct(id);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.NO_CONTENT
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "/delete")
	@ResponseStatus(value = HttpStatus.NO_CONTENT)
	public ResponseEntity<Response<?>> deleteMulti(
			@RequestBody @Valid List<String> ids,
		    HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			Error error = cartService.deleteMulti(ids);
			if(!error.getCode().toString().equals(ErrorCode.SUCCESS.getCode()))
				return new ResponseEntity<>(
						new Response<Error>().error(transId, error),
						HttpStatus.BAD_REQUEST
				);
			
			return new ResponseEntity<>(
					new Response<String>().success(
							transId,
							ErrorCode.SUCCESS.getEn()
					),
					HttpStatus.NO_CONTENT
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
		    throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
