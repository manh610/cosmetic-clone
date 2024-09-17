package com.cosmetic.gg.dto.response.order;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class orderProductItemResponse {

	private String productItemId;
	
	private Integer quantity;
}
